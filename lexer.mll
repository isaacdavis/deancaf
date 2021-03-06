{
open Lexing
open Parser
open String

exception ForbiddenWordError of string
exception SyntaxError of string

let string_buf = ref []
let backslashes = ref 0

let raise_forbidden_error lexbuf =
  let pos = Lexing.lexeme_start_p lexbuf in
  let lnum = string_of_int pos.pos_lnum in
  let column = string_of_int (1 + pos.pos_cnum - pos.pos_bol) in
  let token = Lexing.lexeme lexbuf in
  let s = "Forbidden word: line " ^ lnum ^ ", column " ^ column ^ ": " ^ token ^
    "\n" in
  raise (ForbiddenWordError(s))

let raise_syntax_error lexbuf =
  let pos = Lexing.lexeme_start_p lexbuf in
  let lnum = string_of_int pos.pos_lnum in
  let column = string_of_int (1 + pos.pos_cnum - pos.pos_bol) in
  let token = Lexing.lexeme lexbuf in
  let s = "Syntax error: line " ^ lnum ^ ", column " ^ column ^ ": " ^ token ^
    "\n" in
  raise (SyntaxError(s))

let parse_char lexbuf =
  (* TODO we shouldn't ever really hit error cases here, right? *)
  let s = Lexing.lexeme lexbuf in
  match length s with
  | 4 ->
    (match get s 1 with
    | '\\' ->
      (match get s 2 with
      | 't' -> '\t'
      | 'n' -> '\n'
      | c -> c)
    | _ -> raise_syntax_error lexbuf)
  | 3 -> get s 1
  | _ -> raise_syntax_error lexbuf

let parse_char_in_string lexbuf =
  (* TODO we shouldn't ever really hit error cases here, right? *)
  let s = Lexing.lexeme lexbuf in
  match length s with
  | 2 ->
    (match get s 0 with
    | '\\' ->
      (match get s 1 with
      | 't' -> '\t'
      | 'n' -> raise_syntax_error lexbuf
      | c -> c)
    | _ -> raise_syntax_error lexbuf)
  | 1 -> get s 0
  | _ -> raise_syntax_error lexbuf
}

let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

let lower = ['a'-'z']
let upper = ['A'-'Z']
let letter = lower | upper
let number = ['0'-'9']

let id = ('_' | letter) (letter | number | '_')*  

let integer = '0' | (['1'-'9'] number*)
let char = '\'' ([^'\\''\n''\''] | '\\'[^'n''t'] | "\\n" | "\\t") '\''
let charinstring = [^'\\''\n'] | '\\'[^'n''t'] | "\\n" | "\\t"

rule read = parse
  (* Whitespace *)
  | whitespace  { read lexbuf }
  | newline     { new_line lexbuf; read lexbuf }

  (* Decaf keywords *)
  | "break"         { BREAK }
  | "class"         { CLASS }
  | "continue"      { CONTINUE }
  | "else"          { ELSE }
  | "extends"       { EXTENDS }
  | "if"            { IF }
  | "new"           { NEW }
  | "private"       { PRIVATE }
  | "protected"     { PROTECTED }
  | "public"        { PUBLIC }
  | "return"        { RETURN }
  | "static"        { STATIC }
  | "super"         { SUPER }
  | "this"          { THIS }
  | "while"         { WHILE }

  (* Decaf forbidden words *)
  | "abstract"      { raise_forbidden_error lexbuf }
  | "byte"          { raise_forbidden_error lexbuf }
  | "case"          { raise_forbidden_error lexbuf }
  | "catch"         { raise_forbidden_error lexbuf }
  | "const"         { raise_forbidden_error lexbuf }
  | "default"       { raise_forbidden_error lexbuf }
  | "do"            { raise_forbidden_error lexbuf }
  | "double"        { raise_forbidden_error lexbuf }
  | "final"         { raise_forbidden_error lexbuf }
  | "finally"       { raise_forbidden_error lexbuf }
  | "for"           { raise_forbidden_error lexbuf }
  | "implements"    { raise_forbidden_error lexbuf }
  | "import"        { raise_forbidden_error lexbuf }
  | "instanceof"    { raise_forbidden_error lexbuf }
  | "interface"     { raise_forbidden_error lexbuf }
  | "long"          { raise_forbidden_error lexbuf }
  | "native"        { raise_forbidden_error lexbuf }
  | "goto"          { raise_forbidden_error lexbuf }
  | "package"       { raise_forbidden_error lexbuf }
  | "short"         { raise_forbidden_error lexbuf }
  | "switch"        { raise_forbidden_error lexbuf }
  | "synchronized"  { raise_forbidden_error lexbuf }
  | "throw"         { raise_forbidden_error lexbuf }
  | "throws"        { raise_forbidden_error lexbuf }
  | "transient"     { raise_forbidden_error lexbuf }
  | "try"           { raise_forbidden_error lexbuf }
  | "volatile"      { raise_forbidden_error lexbuf }

  | "byvalue"       { raise_forbidden_error lexbuf }
  | "cast"          { raise_forbidden_error lexbuf }
  | "future"        { raise_forbidden_error lexbuf }
  | "generic"       { raise_forbidden_error lexbuf }
  | "inner"         { raise_forbidden_error lexbuf }
  | "none"          { raise_forbidden_error lexbuf }
  | "operator"      { raise_forbidden_error lexbuf }
  | "outer"         { raise_forbidden_error lexbuf }
  | "rest"          { raise_forbidden_error lexbuf }
  | "var"           { raise_forbidden_error lexbuf }

  | "byte"          { raise_forbidden_error lexbuf }
  | "double"        { raise_forbidden_error lexbuf }
  | "float"         { raise_forbidden_error lexbuf }
  | "long"          { raise_forbidden_error lexbuf }
  | "short"         { raise_forbidden_error lexbuf }

  | "~"             { raise_forbidden_error lexbuf }
  | "?"             { raise_forbidden_error lexbuf }
  | ":"             { raise_forbidden_error lexbuf }
  | "++"            { raise_forbidden_error lexbuf }
  | "--"            { raise_forbidden_error lexbuf }
  | "&"             { raise_forbidden_error lexbuf }
  | "|"             { raise_forbidden_error lexbuf }
  | "^"             { raise_forbidden_error lexbuf }
  | "<<"            { raise_forbidden_error lexbuf }
  | ">>"            { raise_forbidden_error lexbuf }
  | ">>>"           { raise_forbidden_error lexbuf }
  | "+="            { raise_forbidden_error lexbuf }
  | "-+"            { raise_forbidden_error lexbuf }
  | "*="            { raise_forbidden_error lexbuf }
  | "/+"            { raise_forbidden_error lexbuf }
  | "&="            { raise_forbidden_error lexbuf }
  | "|="            { raise_forbidden_error lexbuf }
  | "^="            { raise_forbidden_error lexbuf }
  | "%="            { raise_forbidden_error lexbuf }
  | "<<="           { raise_forbidden_error lexbuf }
  | ">>="           { raise_forbidden_error lexbuf }
  | ">>>="          { raise_forbidden_error lexbuf }

  | char            { CHAR(parse_char lexbuf) }
  | "\""            { string_buf := []; backslashes := 0; read_string lexbuf }
  | "true"          { BOOL(true)}
  | "false"         { BOOL(false)}
  | "null"          { NULL }

  (* Primitive types *)

  | "boolean"       { BOOLTYPE }
  | "char"          { CHARTYPE }
  | "int"           { INTTYPE }
  | "void"          { VOIDTYPE }

  (* Comments *)
  | "//"[^'\r''\n']*  { read lexbuf }
  | "/*"              { swallow lexbuf }

  (* Identifier *)
  | id              { ID(Lexing.lexeme lexbuf)}

  (* Literals *)
  | integer         { INT(int_of_string (Lexing.lexeme lexbuf))}

  (* Punctuation *)
  | "("             { LPAREN }
  | ")"             { RPAREN }
  | "{"             { LBRACE }
  | "}"             { RBRACE }
  | "["             { LBRACKET }
  | "]"             { RBRACKET }
  | ";"             { SEMICOLON }
  | ","             { COMMA }
  | "."             { DOT }

  (* Operators *)
  | "="             { ASGN }
  | ">"             { GREATER }
  | "<"             { LESS }
  | "!"             { NOT }
  | "=="            { EQUALS }
  | ">="            { GEQ }
  | "<="            { LEQ }
  | "!="            { NEQ }
  | "+"             { PLUS }
  | "-"             { MINUS }
  | "*"             { TIMES }
  | "/"             { DIV }
  | "&&"            { AND }
  | "||"            { OR }
  | "%"             { MOD } 

  | eof             { EOF }

  | _               { raise_syntax_error lexbuf }
  
  and swallow = parse
  | "*/"            { read lexbuf }
  | newline         { new_line lexbuf; swallow lexbuf }
  | eof             { EOF }
  | _               { swallow lexbuf }

  and read_string = parse
  | "\""            { if (!backslashes == 0) || (!backslashes mod 2) != 0 then
                        STRING(String.concat "" (List.rev(!string_buf)))
                      else
                        raise_syntax_error lexbuf
                    }
  (* TODO the special-casing of \n and \t is redundant with
     charinstring/parse_char_in_string *)
  | "\\n"           { string_buf := (String.make 1 '\\') :: !string_buf;
                      string_buf := (String.make 1 'n') :: !string_buf;
                      read_string lexbuf }
  | "\\t"           { string_buf := (String.make 1 '\\') :: !string_buf;
                      string_buf := (String.make 1 't') :: !string_buf;
                      read_string lexbuf }
  | charinstring    { string_buf := (String.make 1
                        (parse_char_in_string lexbuf)) :: !string_buf;
                      read_string lexbuf }
  | '\\'            { backslashes := !backslashes + 1; read_string lexbuf }
  | eof             { EOF }
  | _               { read_string lexbuf }
{}