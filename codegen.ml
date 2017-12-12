
open Ast
open Icode
open Offsetgen
open Runtime
open Symboltable
open Typechecker

let data_header = ".data\n\n"
let text_header = ".text\n.globl _V$String\n.globl _$DecafMain\n\n"

let default_epilogue = "mov $0, %eax\nleave\nret\n"

let out_channel = ref stdout

let output_newlines n = for _ = 1 to n do output !out_channel "\n" 0 1 done
let out_str s = output !out_channel s 0 (String.length s)

let str_icLiteral = function
  | IntLiteral i -> "$0x" ^ (Printf.sprintf "%x" i)
  | CharLiteral c -> "$" ^ (Printf.sprintf "%d" (Char.code c))
  | BoolLiteral b -> "$" ^ (Printf.sprintf "%x" (if b then 1 else 0))
  | NullLiteral -> "$0x0"

let str_icRegister = function
  | Eax -> "%eax"
  | Ebx -> "%ebx"
  | Ecx -> "%ecx"
  | Edx -> "%edx"
  | Esi -> "%esi"
  | Edi -> "%edi"
  | Ebp -> "%ebp"
  | Esp -> "%esp"
  | InvalidReg -> "%invalid"

let str_icVal = function
  | LiteralVal lit -> str_icLiteral lit
  | LocalVal offset -> "-0x" ^ (Printf.sprintf "%x" (abs offset)) ^ "(%ebp)"
  | IdVal id ->
    let offset = offset_mgr#lookup id in
    if offset < 0 then
      "-0x" ^ (Printf.sprintf "%x" (abs offset)) ^ "(%ebp)"
    else
      "0x" ^ (Printf.sprintf "%x" offset) ^ "(%ebp)"
  | VerbatimVal s -> s
  | RegisterVal r -> str_icRegister r

(* TODO will have to variably size instructions if I make data variably sized *)
let str_icBinOp = function
  | Move -> "movl"
  | Add -> "addl"
  | Sub -> "subl"
  | Mult -> "imull"
  | Div -> "idivl"
  | And -> "andl"
  | Or -> "orl"
  | Mod -> "divl"
  | Xor -> "xorl"

let str_icUnOp = function
  | Pos -> failwith "this is more complicated than a single instruction"
  | Neg -> "negl"
  | Not -> failwith "this is actually a binop"

let out_binExpr binop v1 v2 =
  let b_str = str_icBinOp binop in
  let v1_str = str_icVal v1 in
  let v2_str = str_icVal v2 in
  out_str (b_str ^ " " ^ v1_str ^ ", " ^ v2_str);
  output_newlines 1

let out_unExpr unop v =
  let u_str = str_icUnOp unop in
  let v_str = str_icVal v in
  out_str (u_str ^ " " ^ v_str);
  output_newlines 1

let out_push v =
  let v_str = str_icVal v in
  out_str ("pushl " ^ v_str);
  output_newlines 1

let out_call v =
  let v_str = str_icVal v in
  out_str ("call " ^ v_str);
  output_newlines 1

let str_icUnStatement unop v =
  match unop with
  | Pos -> () (* TODO *)
  | Neg ->
    out_unExpr Neg v
  | Not ->
    out_binExpr Xor (LiteralVal(IntLiteral(1))) v

let register_needed v1 v2 =
  match v1 with
  | LiteralVal l -> false
  | _ -> true

(* TODO fix to do what's needed with div, etc. *)
let str_icBinStatement binop v1 v2 =

  if register_needed v1 v2 then begin
    out_binExpr Move v1 (RegisterVal(Ebx));
    out_binExpr binop (RegisterVal(Ebx)) v2
  end
  else begin
    out_binExpr binop v1 v2
  end

  (* match binop with
  | Move ->
    if register_needed v1 v2 then
    out_str "movl "
    out_str (str)
  | Add -> "addl "
  | Sub -> "subl "
  | Mult -> "imull "
  | Div -> "idivl "
  | And -> "andl "
  | Or -> "orl "
  | Mod -> "divl " *)

let str_icArrayStatement dest v i = () (* TODO *)

let walk_statement mr statement =
  match statement with
  | BinStatement (binop, v1, v2) ->
    str_icBinStatement binop v1 v2

  | UnStatement (unop, v) ->
    str_icUnStatement unop v

  | ArrayStatement (dest, v, i) ->
    str_icArrayStatement dest v i

  | NewArrayStatement (t, dims, dim) -> ()
  | NewObjStatement(dest, cr, args) -> ()
  | MethodCallStatement (dest, callee, offset, args) -> ()

  | StaticMethodCallStatement (dest, idval, args) ->
    List.iter out_push (List.rev args);
    out_call idval;
    out_binExpr Move (RegisterVal(Eax)) dest


  | FieldAccessStatement (dest, callee, offset) -> ()
  | IfStatement (tbl, loc , cond, statements) -> ()
  | WhileStatement (tbl, cond, statements) -> ()
  | ReturnStatement (v_o) -> out_str "leave\nret\n"
  | ContinueStatement -> ()
  | BreakStatement -> ()
  | SuperStatement args -> ()

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
let walk_method (cr : classRecord) mr =
  if not (is_runtime_class cr.name) then begin
    out_str (mr.name ^ ":\n");
    out_str "push %ebp\n";
    out_str "mov %ebp, %esp\n";
    if mr.size > 0 then
      out_binExpr Sub (LiteralVal(IntLiteral(mr.size))) (RegisterVal(Esp));
    offset_mgr#push mr.local_offset_table;    
    List.iter (walk_statement mr) mr.statements;

    if (List.length mr.statements != 0) then
      (match List.hd (List.rev mr.statements) with
      | ReturnStatement _ -> ()
      | _ -> out_str default_epilogue)
    else
      out_str default_epilogue;

    ignore(offset_mgr#pop);
    output_newlines 1
  end

let write_text_section n c =
  let cr = class_record_table#get n in
  let iter_fun k v =
    (* TODO this does not support multiple methods called "main" and is therefore a dirty hack *)
    if String.compare k "main" == 0 then out_str (main_method_name ^ ":\n");
    let m = method_frame_table#get (mangle_name cr.name k false) in
    walk_method cr m
  in
  c.methodTable#iter iter_fun;
  (* Constructor *)
  walk_method cr (method_frame_table#get (mangle_name cr.name "" true))

let print_string_literals () =
  let print_string_literal k v =
    out_str (k ^ ": .ascii \"" ^ v ^ "\"");
    output_newlines 1
  in
  string_literal_table#iter print_string_literal;
  output_newlines 1

let gen_code tree oc =
  out_channel := oc;
  output !out_channel data_header 0 (String.length data_header);
  tree#iter write_data_section;
  print_string_literals ();
  output !out_channel text_header 0 (String.length text_header);
  tree#iter write_text_section
