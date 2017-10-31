type astBinOp =
    Asgn
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

type astUnOp =
    UnPlus
  | UnMinus
  | Not

type astModifier =
    Static
  | Public
  | Private
  | Protected
  
type astType =
    ClassType of string
  | ArrayType of astType (* TODO Dimension? *)
  | BoolType
  | CharType
  | IntType
  | VoidType
  | NullType
  (* TODO init, meta? *)

type astLiteral =
    NullLiteral
  | BoolLiteral of bool
  | IntLiteral of int
  | CharLiteral of char
  | StringLiteral of string

type astFormal = {name: string; t: astType}

type astSuper = {super: astType}

type astNewArrayExpr = {t: astType; dimList: astExpr list}

and astNonNewArrayExpr =
    LiteralExpr of astLiteral
  | ThisExpr
  | ParenExpr of astExpr
  | NewObjExpr of string * astExpr list
  | ThisCallExpr of string * astExpr list
  | MethodCallExpr of astPrimary * string * astExpr list
  | SuperCallExpr of string * astExpr list
  | ArrayExpr of astPrimary * astExpr
  | FieldExpr of astPrimary * string
  | SuperFieldExpr of string

and astExpr =
    UnOpExpr of astUnOp * astExpr
  | BinOpExpr of astBinOp * astExpr * astExpr
  | PrimaryExpr of astPrimary

and astPrimary =
    NewArrayPrimary of astNewArrayExpr
  | NonNewArrayPrimary of astNonNewArrayExpr
  | IdPrimary of string

type astVarDecl = {name: string; expr: astExpr option}

type astStatement = 
    EmptyStatement
  | DeclStatement of astType * astVarDecl list
  | IfStatement of astExpr * astStatement * astStatement option
  | ExprStatement of astExpr
  | WhileStatement of astExpr * astStatement
  | ReturnStatement of astExpr option
  | ContinueStatement
  | BreakStatement
  | BlockStatement of astStatement list
  | SuperStatement of astExpr list
  
type astMember =
    Field of astModifier list * astType * astVarDecl list
  | Method of string * astType * astModifier list * astFormal list * astStatement list
  | Constructor of string * astType * astModifier list * astFormal list * astStatement list

type astClass = {name: string; super: astSuper; memberList: astMember list}





