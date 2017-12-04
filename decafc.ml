open Lexing
open Parser
open Lexer
open Ast
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
 
  Runtime.setup ();

  (* Read from stdin in a loop for now *)
  let lexbuf = Lexing.from_channel stdin in
  (try Parser.classList Lexer.read lexbuf with
        | Lexer.SyntaxError s -> print_string(s); exit 1
        | Lexer.ForbiddenWordError s -> print_string(s); exit 1
        | Parsing.Parse_error ->
            printParseError lexbuf;
            exit 1);

  let errs = type_check class_table in

  if List.length errs != 0 then begin
    List.iter print_endline (List.rev errs);
    exit 1
  end;

  exit 0

;;

(* Wheeeeeeee gogogo! *)
main()