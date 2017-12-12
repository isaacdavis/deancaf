open Lexing
open Sys

open Ast
open Codegen
open Lexer
open Icodegen
open Parser
open Typechecker

(*
  Compiler driver - contains main function
*)

(*
  TODO add type annotations everywhere
  TODO add comments
  TODO enforce access violations in type-checker
  TODO add nulltype
  TODO enforce return types in type-checker
  TODO enforce presence of 1 public static void main in type-checker
  TODO make variable name styling consistent
  TODO make sure it's safe to modify an argument to a function within the
    function
  TODO transfer modules to signatures/mli files
  TODO add support for c-style array syntax (i.e. int arr[] instead of int[]
    arr)
  TODO check constructor modifiers in type-checker
  TODO make sure continue and break are only in loops in type-checker
  TODO implement different data sizes instead of just 4 bytes for everything
  TODO can there be a static and non-static method with the same name in the
    same class?
  TODO make asm printing of DecafMain less blatantly shitty
  TODO support local field accesses without "this" in front of them, or bar
    these accesses entirely - at any rate, don't crash
  TODO enforce 80-char limit

  Tuesday:

  1 hr:
  Comment code reasonably
  Resolve any todos that seem easy
  Sanitize todos but leave them in?
  Add todos to blatantly unfinished parts
  
  1 hr:
  Write some illustrative tests/prepare a presentation
  Print everything out/email handin to spr

  More time:

  Working by 9 -> 5 hours of work until 2:30
  -> 3 hours of this stuff:
  Get if statements working
  Get while loops working
  Get field access working (easy)
  Get array assignment/field assignment working
*)

(*
  Given the current state of lexbuf, prints a useful parse error with line
  number, column, and token.
*)
let printParseError lexbuf =
  let pos = Lexing.lexeme_start_p lexbuf in
  let lnum = string_of_int pos.pos_lnum in
  let column = string_of_int (1 + pos.pos_cnum - pos.pos_bol) in
  let token = Lexing.lexeme lexbuf in
  print_string("Parse error: line " ^ lnum ^ ", column " ^ column ^ ": " ^
    token ^ "\n")

(*
  Entry point for compiler driver.
*)
let main () =

  if (Array.length Sys.argv) != 3 then
  begin
    print_endline "Usage: decafc <input file> <output file>";
    exit 1
  end;

  let in_filename = Sys.argv.(1) in
  let out_filename = Sys.argv.(2) in
  if (String.compare in_filename out_filename) == 0 then
  begin
    print_endline "Input and output files cannot be the same";
    exit 1
  end;

  let in_channel = open_in in_filename in

  Runtime.setup ();

  let lexbuf = Lexing.from_channel in_channel in
  let sorted_classes = (try Parser.classList Lexer.read lexbuf with
        | Lexer.SyntaxError s -> print_string(s); exit 1
        | Lexer.ForbiddenWordError s -> print_string(s); exit 1
        | Parsing.Parse_error ->
            printParseError lexbuf;
            exit 1)
  in
  close_in in_channel;

  let errs = type_check class_table in
  if List.length errs != 0 then begin
    List.iter print_endline (List.rev errs);
    exit 1
  end;

  let all_sorted_classes = Runtime.add_runtime_classes sorted_classes in

  gen_icode all_sorted_classes;

  let asm_filename = out_filename ^ ".s" in
  let asm_channel = open_out asm_filename in

  gen_code class_table asm_channel;

  close_out asm_channel;

  exit (Sys.command ("gcc -m32 -o " ^ out_filename ^ " " ^ asm_filename ^ " " ^
    "runtime.c"));
;;

main()