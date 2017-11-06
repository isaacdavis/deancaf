{
open Lexing
open Parser
open String

exception ForbiddenWordError of string
exception SyntaxError of string
exception Eof

let raise_forbidden_error lexbuf =
  raise (ForbiddenWordError("Forbidden word: " ^ Lexing.lexeme lexbuf ^ "\n"))

let raise_syntax_error lexbuf =
  raise (SyntaxError("Syntax Error: " ^ Lexing.lexeme lexbuf ^ "\n"))

}

let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let comment = "/*"([^'*'] | ('*'+ [^'/']))*"*/"

let lower = ['a'-'z']
let upper = ['A'-'Z']
let letter = lower | upper
let number = ['0'-'9']

let id = ('_' | letter) (letter | number | '_')*  

let integer = '0' | (['1'-'9'] number*)
let char = '\'' [^'\\''\n''\''] | '\\'[^'n''t'] | "\\n" | "\\t" '\''
(* TODO fix string regex *)
let string = '\"' ([^'\n''\"''\\'] | ('\\''\\')*'\\''\"')*'\"'

rule read =
  parse
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

  (* TODO fix char value*)
  | char            { CHAR(get (Lexing.lexeme lexbuf) 0) }
  | string          { STRING(Lexing.lexeme lexbuf)}
  | "true"          { BOOL(true)}
  | "false"         { BOOL(false)}
  | "null"          { NULL }

  (* Primitive types *)

  | "boolean"       { BOOLTYPE }
  | "char"          { CHARTYPE }
  | "int"           { INTTYPE }
  | "void"          { VOIDTYPE }

  (*Comments*)
  | "//"[^'\r''\n']* { new_line lexbuf; read lexbuf}
  | comment         { read lexbuf}

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
  
{}