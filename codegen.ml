
open Ast
open Icode
open Icodegen
open Runtime
open Symboltable
open Typechecker

(*
  32-bit x86 code generation from icode
*)

let data_header = ".data\n\n"
let text_header = ".text\n.globl _V$String\n.globl _$DecafMain\n\n"

(* default function epilogue *)
let default_epilogue = "mov $0, %eax\nleave\nret\n"

(* Channel to write to *)
let out_channel = ref stdout

(* output primitives *)
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

(* constructs a complicated address offset i.e. 0x4(%ebx, %ecx, 0x8) *)
let str_offset front_offset base_reg offset_reg scale =
  if (front_offset < 0) || (scale < 0) then
    failwith "str_offset: front_offset and scale should not be negative here";

  let str_base_reg = str_icRegister base_reg in
  let str_offset_reg = str_icRegister offset_reg in
  let str_scale = Printf.sprintf "0x%x" scale in
  let str_front_offset = Printf.sprintf "0x%x" front_offset in

  str_front_offset ^ "(" ^ str_base_reg ^ ", " ^ str_offset_reg ^ ", " ^
    str_scale ^ ")"

(* Constructs a simple address offset i.e. 0x4(%ebx) *)
let str_simple_offset offset reg =
  if (offset < 0) then
    failwith "str_simple_offset: offset should not be negative here";
  let str_reg = str_icRegister reg in
  let str_offset = Printf.sprintf "0x%x" offset in

  str_offset ^ "(" ^ str_reg ^ ")"

let str_icVal = function
  | LiteralVal lit -> str_icLiteral lit
  | LocalVal offset ->
    if offset < 0 then
      (Printf.sprintf "-0x%x" (abs offset)) ^ "(%ebp)"
    else
      (Printf.sprintf "0x%x" offset) ^ "(%ebp)"
  | IdVal id ->
    let offset = offset_mgr#lookup id in
    if offset < 0 then
      (Printf.sprintf "-0x%x" (abs offset)) ^ "(%ebp)"
    else
      (Printf.sprintf "0x%x" offset) ^ "(%ebp)"
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
  | Lea -> "leal"
  | _ -> failwith "conditionals not supported for now"

let str_icUnOp = function
  | Pos -> failwith "unary pos not supported for now"
  | Neg -> "negl"
  | Not -> failwith "unary not should have been converted to xor"

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

let out_call_indirect v =
  let v_str = str_icVal v in
  out_str ("call *" ^ v_str);
  output_newlines 1

let write_icUnStatement unop v =
  match unop with
  | Pos -> () (* TODO *)
  | Neg ->
    out_unExpr Neg v
  | Not ->
    out_binExpr Xor (LiteralVal(IntLiteral(1))) v

(* Determines if we need to use a register intermediary when doing a binop *)
(* TODO is this exhaustive? Probably not *)
let register_needed v1 v2 =
  match v1 with
  | LiteralVal _ -> false
  | RegisterVal _ -> false
  | _ ->
    (match v2 with
    | RegisterVal _ -> false
    | _ -> true)

(* TODO fix to support division, multiplication, etc. *)
let write_icBinStatement binop v1 v2 =

  if register_needed v1 v2 then begin
    out_binExpr Move v1 (RegisterVal(Ebx));
    out_binExpr binop (RegisterVal(Ebx)) v2
  end
  else begin
    out_binExpr binop v1 v2
  end

let write_icArrayStatement dest v i =
  (* TODO array access works but not assignment *)
  let ebx = RegisterVal(Ebx) in
  let ecx = RegisterVal(Ecx) in
  let offset = str_offset data_sz Ebx Ecx data_sz in
  out_binExpr Move v ebx;
  out_binExpr Move i ecx;
  let offset_val = VerbatimVal(offset) in
  (* TODO works but is sketchy *)
  if register_needed offset_val dest then begin
    let edx = RegisterVal(Edx) in
    out_binExpr Move offset_val edx;
    out_binExpr Move edx dest
  end
  else
    out_binExpr Move offset_val dest

let write_icNewArrayStatement dest t dims =
  List.iter out_push (List.rev dims);
  out_push (LiteralVal(IntLiteral(List.length dims)));
  (* Enum for _$ArrayAllocate in runtime.c - all types are same size for now *)
  ignore(t);
  out_push (LiteralVal(IntLiteral(2)));
  out_call (VerbatimVal("_$ArrayAllocate"));
  out_binExpr Move (RegisterVal(Eax)) dest

(* Create runtime String object *)
let create_string dest arg =
  let ebx = RegisterVal(Ebx) in
  out_binExpr Lea arg ebx;
  out_push ebx;
  out_call (VerbatimVal("_$CreateString"));
  out_binExpr Move (RegisterVal(Eax)) dest

let write_icNewObjStatement dest (cr : icClassRecord) args =
  if (String.compare cr.name "String") == 0 then
    create_string dest (List.hd args)
  else begin
    let ctor_name = mangle_name cr.name "" true in
    let vt_name = vtable_name cr.name in
    let eax = RegisterVal(Eax) in
    let ebx = RegisterVal(Ebx) in

    out_push(LiteralVal(IntLiteral(cr.size)));
    out_call (VerbatimVal("malloc"));

    let vtable_offset = str_simple_offset (data_sz * 2) Eax in
    out_binExpr Move (VerbatimVal(vt_name)) ebx;
    out_binExpr Move ebx (VerbatimVal(vtable_offset));
    
    out_binExpr Move eax dest;
    List.iter out_push (List.rev args);
    out_push dest;
    out_call (VerbatimVal(ctor_name))
  end

let write_icMethodCallStatement dest callee offset args =
  let ebx = RegisterVal(Ebx) in
  let ecx = RegisterVal(Ecx) in

  out_binExpr Move callee ebx;
  let vtable_offset = str_simple_offset (data_sz * 2) Ebx in
  out_binExpr Move (VerbatimVal(vtable_offset)) ecx;

  out_binExpr Add (LiteralVal(IntLiteral(offset))) ecx;
  List.iter out_push (List.rev args);
  out_push callee;
  out_call_indirect ecx

let walk_statement mr statement =
  match statement with
  | BinStatement (binop, v1, v2) ->
    write_icBinStatement binop v1 v2

  | UnStatement (unop, v) ->
    write_icUnStatement unop v

  | ArrayStatement (dest, v, i) ->
    write_icArrayStatement dest v i

  | NewArrayStatement (dest, t, dims) ->
    write_icNewArrayStatement dest t dims

  | NewObjStatement(dest, cr, args) ->
    write_icNewObjStatement dest cr args

  | MethodCallStatement (dest, callee, offset, args) ->
    write_icMethodCallStatement dest callee offset args

  | StaticMethodCallStatement (dest, idval, args) ->
    List.iter out_push (List.rev args);
    out_call idval;
    out_binExpr Move (RegisterVal(Eax)) dest

  | FieldAccessStatement (dest, callee, offset) -> () (* TODO *)

  | IfStatement (tbl, loc , cond, statements) -> () (* TODO *)

  | WhileStatement (tbl, cond, statements) -> () (* TODO *)

  | ReturnStatement (v_o) ->
    (match v_o with
    | Some v ->
      out_binExpr Move v (RegisterVal(Eax))
    | None -> ());
    out_str "leave\nret\n"

  | ContinueStatement -> () (* TODO *)

  | BreakStatement -> () (* TODO *)

  | SuperStatement args -> () (* TODO *)

(* Writes vtables, etc. *)
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

let walk_method (cr : icClassRecord) mr =
  if not (is_runtime_class cr.name) then begin
    let ebp = RegisterVal(Ebp) in
    let esp = RegisterVal(Esp) in
    out_str (mr.name ^ ":\n");
    out_push ebp;
    out_binExpr Move esp ebp;
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

(* Writes methods, etc. *)
let write_text_section n c =
  let cr = class_record_table#get n in
  let iter_fun k v =
    (* TODO this does not support multiple methods called "main" and is
       therefore a dirty hack *)
    if String.compare k "main" == 0 then out_str (main_method_name ^ ":\n");
    let m = method_frame_table#get (mangle_name cr.name k false) in
    walk_method cr m
  in
  c.methodTable#iter iter_fun;
  (* Constructor *)
  walk_method cr (method_frame_table#get (mangle_name cr.name "" true))

(* Writes string literals *)
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
