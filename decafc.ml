open Lexing
open Parser
open Lexer

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

  (* Read from stdin in a loop for now*)
  let lexbuf = Lexing.from_channel stdin in
      try
        while true do
            let _ = Parser.classList Lexer.read lexbuf in
            print_newline();
            flush stdout
        done
      with
        | Lexer.SyntaxError s -> print_string(s); exit 1
        | Lexer.ForbiddenWordError s -> print_string(s); exit 1
        | Parsing.Parse_error ->
            printParseError lexbuf;
            exit 1
        | Lexer.Eof -> exit 0
;;

(* Wheeeeeeee gogogo! *)
main()