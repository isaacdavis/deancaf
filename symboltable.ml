module StrMap = Map.Make(struct type t = string let compare = compare end)

class ['a] symbol_table = object

  val mutable table = StrMap.empty

  method put (k : string) (v : 'a) =
    table <- StrMap.add k v table

  method get (k : string) :'a = StrMap.find k table

end

class ['a] symbol_table_manager = object

  val stack : 'a Stack.t = Stack.create ()

  method push (s : 'a symbol_table) =
    Stack.push s stack

  method pop (s : 'a symbol_table) =
    Stack.pop stack

end

(* let walk_binop = function
  | Asgn
  | Greater
  | Less
  | Equals
  | Geq
  | Leq
  | Neq
  | BinPlus
  | BinMinus
  | Times
  | Div
  | And
  | Or
  | Mod

let walk_unop = function
  | UnPlus
  | UnMinus
  | Not

let walk_literal = function
  | NullLiteral
  | BoolLiteral of bool
  | IntLiteral of int
  | CharLiteral of char
  | StringLiteral of string

let walk_formal formal = 1

let walk_newarr newarr = 1

let walk_nonnew = function
  | LiteralExpr of astLiteral
  | ThisExpr
  | ParenExpr of astExpr
  | NewObjExpr of string * astExpr list
  | ThisCallExpr of string * astExpr list
  | MethodCallExpr of astPrimary * string * astExpr list
  | SuperCallExpr of string * astExpr list
  | ArrayExpr of astPrimary * astExpr
  | FieldExpr of astPrimary * string
  | SuperFieldExpr of string

let walk_expr = function
  | UnOpExpr of astUnOp * astExpr
  | BinOpExpr of astBinOp * astExpr * astExpr
  | PrimaryExpr of astPrimary

let walk_primary = function
  | NewArrayPrimary of astNewArrayExpr
  | NonNewArrayPrimary of astNonNewArrayExpr
  | IdPrimary of string

let walk_vardecl vardecl = 1

let walk_statement = function
  | EmptyStatement
  | DeclStatement of astType * astVarDecl list
  | IfStatement of astType symbol_table * astExpr * astStatement * astStatement option
  | ExprStatement of astExpr
  | WhileStatement of astType symbol_table * astExpr * astStatement
  | ReturnStatement of astExpr option
  | ContinueStatement
  | BreakStatement
  | BlockStatement of astType symbol_table * astStatement list
  | SuperStatement of astExpr list

let walk_member = function
  | Field of astModifier list * astType * astVarDecl
  | Method of string * astType * astType symbol_table * astModifier list * astFormal list * astStatement list
  | Constructor of string * astType * astModifier list * astFormal list * astStatement list

let walk_class c = 1   *)