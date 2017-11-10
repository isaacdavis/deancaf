
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

let strBinOp = function
  | Asgn -> "="
  | Greater -> ">"
  | Less -> "<"
  | Equals -> "="
  | Geq -> ">="
  | Leq -> "<="
  | Neq -> "!="
  | BinPlus -> "bin+"
  | BinMinus -> "bin-"
  | Times -> "*"
  | Div -> "/"
  | And -> "&&"
  | Or -> "||"
  | Mod -> "%"

type astUnOp =
    UnPlus
  | UnMinus
  | Not

let strUnOp = function
  | UnPlus -> "un+"
  | UnMinus -> "un-"
  | Not -> "!"

type astModifier =
    Static
  | Public
  | Private
  | Protected
  
let strModifier = function
  | Static -> "static"
  | Public -> "public"
  | Private -> "private"
  | Protected -> "protected"

type astType =
    ClassType of string
  | ArrayType of astType * int (* TODO Dimension? *)
  | BoolType
  | CharType
  | IntType
  | VoidType
  | NullType
  (* TODO init, meta? *)

let rec strType = function
  | ClassType s -> "classT(" ^ s ^ ")"
  | ArrayType (ty, n) -> "arrayT(" ^ strType ty ^ ", " ^ string_of_int n ^ ")"
  | BoolType -> "boolT"
  | CharType -> "charT"
  | IntType -> "intT"
  | VoidType -> "voidT"
  | NullType -> "nullT"

type astLiteral =
    NullLiteral
  | BoolLiteral of bool
  | IntLiteral of int
  | CharLiteral of char
  | StringLiteral of string

let strLiteral = function
  | NullLiteral -> "null"
  | BoolLiteral b -> string_of_bool b
  | IntLiteral i -> string_of_int i
  | CharLiteral c -> String.make 1 c
  | StringLiteral s -> s

type astFormal = {name: string; t: astType}

let strFormal f =
  "formal(" ^ f.name ^ ", " ^ strType f.t ^ ")"

type astSuper = {super: astType}

let strSuper s =
  "super(" ^ strType s.super ^ ")"

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

let rec strExpr = function
  | UnOpExpr (op, e) -> "unOpExpr(" ^ strUnOp op ^ ", " ^ strExpr e ^ ")"
  | BinOpExpr (op, a, b) -> "binOpExpr(" ^ strBinOp op ^ ", " ^ strExpr a ^ ", " ^ strExpr a ^ ")"
  | PrimaryExpr p -> "primaryExpr(" ^ strPrimary p ^ ")"

and strNewArrayExpr n =
  let strAppend a x =
    a ^ strExpr x ^ "; " in
  "newArrayExpr(" ^ strType n.t ^ ", " ^ List.fold_left strAppend "" n.dimList ^ ")"

and strNonNewArrayExpr n =
  let strAppend a x =
    a ^ strExpr x ^ "; " in
  match n with
  | LiteralExpr l -> "literalExpr(" ^ strLiteral l ^ ")"
  | ThisExpr -> "thisExpr"
  | ParenExpr e -> "parenExpr(" ^ strExpr e ^ ")"
  | NewObjExpr (s, eList) -> "newObjExpr(" ^ s ^ ", " ^ List.fold_left strAppend "" eList ^ ")"
  | ThisCallExpr (s, eList) -> "thisCallExpr(" ^ s ^ ", " ^ List.fold_left strAppend "" eList ^ ")"
  | MethodCallExpr (p, s, eList) -> "methodCallExpr(" ^ strPrimary p ^ ", " ^ s ^ ", " ^ List.fold_left strAppend "" eList ^ ")"
  | SuperCallExpr (s, eList) -> "superCallExpr(" ^ s ^ ", " ^ List.fold_left strAppend "" eList ^ ")"
  | ArrayExpr (p, e) -> "arrayExpr(" ^ strPrimary p ^ ", " ^ strExpr e ^ ")"
  | FieldExpr (p, s) -> "fieldExpr(" ^ strPrimary p ^ ", " ^ s ^ ")"
  | SuperFieldExpr s -> "superFieldExpr(" ^ s ^ ")"

and strPrimary = function
  | NewArrayPrimary n -> "newArrayPrimary(" ^ strNewArrayExpr n ^ ")"
  | NonNewArrayPrimary n -> "nonNewArrayPrimary(" ^ strNonNewArrayExpr n ^ ")"
  | IdPrimary s -> "idPrimary(" ^ s ^ ")"

let strVarDecl v =
  match v.expr with
    | Some e -> "strVarDecl(" ^ v.name ^ ", " ^ strExpr e ^ ")"
    | None -> "strVarDecl(" ^ v.name ^ ")"

let rec strStatement s =
  let strAppend f a x =
    a ^  f x ^ "; " in
  let strAppendStatement = strAppend strStatement in
  let strAppendExpr = strAppend strExpr in
  let strAppendVarDecl = strAppend strVarDecl in

  match s with
  | EmptyStatement -> "emptyStatement"
  | DeclStatement (t, vList) -> "declStatement(" ^ strType t ^ ", " ^ List.fold_left strAppendVarDecl "" vList ^ ")"
  | IfStatement (e, sA, sO) -> "ifStatement(" ^ strExpr e ^ ", " ^ strStatement sA ^ ", " ^
    (match sO with
    | Some sB -> strStatement sB
    | None -> "")
    ^ ")"
  | ExprStatement e -> "exprStatement(" ^ strExpr e ^ ")"
  | WhileStatement (e, sW) -> "whileStatement(" ^ strExpr e ^ ", " ^ strStatement sW ^ ")"
  | ReturnStatement eO -> "returnStatement(" ^
    (match eO with
    | Some e -> strExpr e
    | None -> "")
    ^ ")"
  | ContinueStatement -> "continueStatement"
  | BreakStatement -> "breakStatement"
  | BlockStatement sList -> "blockStatement(" ^ List.fold_left strAppendStatement "" sList ^ ")"
  | SuperStatement eList -> "superStatement(" ^ List.fold_left strAppendExpr "" eList ^ ")"

let strMember m =
  let strAppend f a x =
    a ^ f x ^ "; " in
  let strAppendModifier = strAppend strModifier in
  let strAppendVarDecl = strAppend strVarDecl in
  let strAppendFormal = strAppend strFormal in
  let strAppendStatement = strAppend strStatement in

  match m with
  | Field (mList, t, vList) -> "field(" ^ List.fold_left strAppendModifier "" mList ^
    ", " ^ strType t ^ ", " ^ List.fold_left strAppendVarDecl "" vList ^ ")"
  | Method (s, t, mList, fList, sList) -> "method(" ^ s ^ "," ^ strType t ^
    "," ^ List.fold_left strAppendModifier "" mList ^ ", " ^
    List.fold_left strAppendFormal "" fList  ^ "," ^
    List.fold_left strAppendStatement "" sList ^ ")"
  | Constructor (s, t, mList, fList, sList) -> "constructor(" ^ s ^ "," ^ strType t ^
    "," ^ List.fold_left strAppendModifier "" mList ^ ", " ^
    List.fold_left strAppendFormal "" fList  ^ "," ^
    List.fold_left strAppendStatement "" sList ^ ")"

let strClass c =
  let strAppend a x =
    a ^ strMember x ^ "; " in
  "class(" ^ c.name ^ ", " ^ strSuper c.super ^ ", " ^ List.fold_left strAppend "" c.memberList ^ ")"