
open Ast
open Icode
open Symboltable

let class_record_table : classRecord symbol_table = new symbol_table
let method_frame_table : icMethod symbol_table = new symbol_table

(* Relative to start of class object *)
let curr_field_offset = ref 0

(* Relative to start of vtable *)
let curr_method_offset = ref 0

(* Relative to ebp within function call *)
let curr_local_offset = ref 0
let curr_param_offset = ref 8

let get_type_size = function
  | ClassType _ -> ptr_sz
  | ArrayType (_, _) -> ptr_sz
  | BoolType -> bool_sz
  | CharType -> char_sz
  | IntType -> int_sz
  | VoidType -> failwith "get_type_size called on VoidType"
  | MethodType (_, _, _) -> ptr_sz (* TODO should this be a failwith? *)

let walk_member c cname member =
  match member with
  | Field (_, t, _) ->
    if not (c.field_offset_table#contains cname) then
    begin
      c.field_offset_table#put cname !curr_field_offset;
      curr_field_offset := !curr_field_offset + get_type_size t
    end
  | Method (mname, _, tbl, mods, formals, _) ->
    if not (List.mem Static mods) &&
      not (c.method_offset_table#contains cname) then
    begin
      c.method_offset_table#put cname !curr_method_offset;
      curr_method_offset := !curr_method_offset + ptr_sz
    end;

    let mangled_name = mangle_name cname mname false in
    let m =
      { name = mangled_name
      ; c = c
      ; size = 0
      ; local_offset_table = new symbol_table
      ; statements = []
      } in
    curr_local_offset := 0;
    (* Space for hidden "this" parameter *)
    curr_param_offset := if List.mem Static mods then 8 else 12;
    let formal_names = List.map (fun (f : astFormal) -> f.name) formals in
    let filter_and_add k v =
      if  List.mem k formal_names then
      begin
        m.local_offset_table#put k !curr_param_offset;
        curr_param_offset := !curr_param_offset + get_type_size v
      end
      else begin
        m.local_offset_table#put k !curr_local_offset;
        curr_local_offset := !curr_local_offset - get_type_size v
      end
    in
    tbl#iter filter_and_add;
    m.size <- !curr_local_offset;
    method_frame_table#put mangled_name m

  | Constructor (_, _, _, _, _) ->
    failwith "attempted to calculate offset on constructor"

let add_super_members c =
  match c.super with
  | None -> ()
  | Some super ->
    let add_to_tbl tbl name offset =
      tbl#put name offset
    in
    let add_to_field_table = add_to_tbl c.field_offset_table in
    let add_to_method_table = add_to_tbl c.method_offset_table in

    super.field_offset_table#iter add_to_field_table;
    super.method_offset_table#iter add_to_method_table;
    curr_field_offset := super.size - ptr_sz;
    curr_method_offset := super.method_offset_table#size * ptr_sz

let walk_class c =
  curr_field_offset := 0;
  curr_method_offset := 0;
  match c.t with
  | ClassType name ->
    let super =
      (match c.super with
      | None -> None
      | Some superc ->
        (match superc.t with
        | ClassType supername -> class_record_table#get_opt supername
        | _ -> failwith "class does not have ClassType")) in
    let cr = 
      { name = name
      ; super = super
      ; size = 0
      ; field_offset_table = new symbol_table
      ; method_offset_table = new symbol_table
      } in
    let add_class n b = walk_member cr n b in
    add_super_members cr;
    c.fieldTable#iter add_class;
    c.methodTable#iter add_class;
    cr.size <- !curr_field_offset + ptr_sz; (* space for vtable ptr as well *)
    class_record_table#put name cr; 

  | _ -> failwith "class does not have ClassType"

let gen_offsets classlist =
  List.iter walk_class classlist;

