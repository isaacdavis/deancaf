
open Ast
open Icode
open Symboltable

let class_record_table : classRecord symbol_table = new symbol_table
let curr_field_offset = ref 0
let curr_method_offset = ref 0

let get_type_size = function
  | ClassType _ -> ptr_sz
  | ArrayType (_, _) -> ptr_sz
  | BoolType -> bool_sz
  | CharType -> char_sz
  | IntType -> int_sz
  | VoidType -> failwith "get_type_size called on VoidType"
  | MethodType (_, _, _) -> ptr_sz (* TODO should this be a failwith? *)

let walk_member c name m =
  match m with
  | Field (_, t, _) ->
    if not (c.field_offset_table#contains name) then
    begin
      c.field_offset_table#put name !curr_field_offset;
      curr_field_offset := !curr_field_offset + get_type_size t
    end
  | Method (_, _, _, _, _, _) ->
    if not (c.method_offset_table#contains name) then
    begin
      c.method_offset_table#put name !curr_method_offset;
      curr_method_offset := !curr_method_offset + ptr_sz
    end
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
        | ClassType supername -> class_record_table#get supername
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
    cr.field_offset_table#iter (fun k v -> print_endline (name ^ " " ^ k ^ " " ^ (string_of_int v)));
    cr.method_offset_table#iter (fun k v -> print_endline (name ^ " " ^ k ^ " " ^ (string_of_int v)));
    cr.size <- !curr_field_offset + ptr_sz; (* space For vtable ptr as well *)
    class_record_table#put name cr

  | _ -> failwith "class does not have ClassType"

let gen_offsets classlist =
  List.iter walk_class classlist;

