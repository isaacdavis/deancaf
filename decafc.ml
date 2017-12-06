open Lexing
open Sys

open Ast
open Lexer
open Offsetgen
open Parser
open Runtime
open Typechecker

(*
  TODO add type annotations everywhere
  TODO add comments
  TODO enforce access violations in type-checker
  TODO add nulltype
  TODO enforce return types in type-checker
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
  print_string("Parse error: line " ^ lnum ^ ", column " ^ column ^ ": " ^ token ^ "\n")

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

  let all_sorted_classes = add_runtime_classes sorted_classes in

  gen_offsets all_sorted_classes;

  let out_filename = Sys.argv.(2) in
  let out_channel = open_out out_filename in
  close_out out_channel;
  exit 0

;;

(* Wheeeeeeee gogogo! *)
main()