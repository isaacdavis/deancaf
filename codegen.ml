
open Ast
open Icode
open Offsetgen
open Runtime
open Symboltable
open Typechecker

let data_header = ".data\n\n"
let text_header = ".text\n.globl _V$String\n.globl _$DecafMain\n\n"

let out_channel = ref stdout

let output_newlines n = for _ = 1 to n do output !out_channel "\n" 0 1 done
let out_str s = output !out_channel s 0 (String.length s)

let walk_statement mr statement =
  match statement with
  | EmptyStatement -> ()
  | DeclStatement (t, decls) -> ()
  | IfStatement (_, _, cond, then_statement, else_statement_o) -> ()
  | ExprStatement e -> ()
  | _ -> () 
  (* | WhileStatement (_,of astType symbol_table * astExpr * astStatement
  | ReturnStatement of astExpr option
  | ContinueStatement
  | BreakStatement
  | BlockStatement of astType symbol_table * astStatement list
  | SuperStatement of astExpr list
 *)
let convert_method_to_icode (cr : classRecord) member =
  if not (is_runtime_class cr.name) then
    match member with
    | Field (_, _, _) -> failwith "code generation called on field"

    | Method (name, _, _, _, _, statements) ->
      let mangled_name = mangle_name cr.name name false in
      let mr = method_frame_table#get mangled_name in
      List.iter (walk_statement mr) statements

    | Constructor (_, _, _, _, statements) ->
      let mangled_name = mangle_name cr.name "" true in
      let mr = method_frame_table#get mangled_name in
      List.iter (walk_statement mr) statements

let ast_to_icode n c =
  let cr = class_record_table#get n in
  let discard_fst _ b = convert_method_to_icode cr b in
  c.methodTable#iter discard_fst;
  convert_method_to_icode cr c.constructor

let write_data_section n c =
  let cr = class_record_table#get n in
  let vt_name = vtable_name cr.name in

  let method_list = ref [] in
  cr.method_offset_table#iter
    (fun k v -> method_list := (k, v) :: !method_list);

  let compare at bt =
    let a = snd at in
    let b = snd bt in
    if a < b then -1 else if a == b then 0 else 1
  in
  method_list := List.sort compare !method_list;
  let print_method_name s =
    out_str ".long ";
    out_str (mangle_name cr.name s false);
    output_newlines 1
  in

  out_str (vt_name ^ ":\n");
  List.iter (fun t -> print_method_name (fst t)) !method_list;
  output_newlines 1

(* TODO rename to avoid naming conflicts with other modules, or make private *)
let walk_member (cr : classRecord) member =
  if not (is_runtime_class cr.name) then
    match member with
    | Field (_, _, _) -> failwith "code generation called on field"

    | Method (name, _, _, _, _, statements) ->
      let method_name =
        if is_main_method member then main_method_name else
          mangle_name cr.name name false in

      out_str (method_name ^ ":");

      output_newlines 1

    | Constructor (_, _, _, _, statements) ->
      out_str ((mangle_name cr.name "" true) ^ ":");
      output_newlines 1

let write_text_section n c =
  let cr = class_record_table#get n in
  let discard_fst _ b = walk_member cr b in
  c.methodTable#iter discard_fst;
  walk_member cr c.constructor



let gen_code tree oc =
  tree#iter ast_to_icode;
  out_channel := oc;
  output !out_channel data_header 0 (String.length data_header);
  tree#iter write_data_section;
  output !out_channel text_header 0 (String.length text_header);
  tree#iter write_text_section
