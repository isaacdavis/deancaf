
open Symboltable

(*
  AST definitions
*)

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
  | ArrayType of astType * int
  | BoolType
  | CharType
  | IntType
  | VoidType
  | MethodType of astType * astType list * astType
  (* TODO null, init, meta? *)

type astLiteral =
    NullLiteral
  | BoolLiteral of bool
  | IntLiteral of int
  | CharLiteral of char
  | StringLiteral of string

type astFormal =
  { name: string
  ; t: astType
  }

type astNewArrayExpr =
  { t: astType
  ; dimList: astExpr list
  }

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

type astVarDecl = 
  { name: string
  ; dim: int
  ; expr: astExpr option
  }

type astStatement = 
    EmptyStatement
  | DeclStatement of astType * astVarDecl list
  | IfStatement of astType symbol_table * astType symbol_table option * astExpr * astStatement * astStatement option
  | ExprStatement of astExpr
  | WhileStatement of astType symbol_table * astExpr * astStatement
  | ReturnStatement of astExpr option
  | ContinueStatement
  | BreakStatement
  | BlockStatement of astType symbol_table * astStatement list
  | SuperStatement of astExpr list
  
type astMember =
    Field of astModifier list * astType * astVarDecl
  | Method of string * astType * astType symbol_table * astModifier list * astFormal list * astStatement list
  | Constructor of astType * astType symbol_table * astModifier list * astFormal list * astStatement list

type astClass =
  { t: astType
  ; super: astClass option
  ; constructor: astMember
  ; fieldTable: astMember symbol_table
  ; methodTable: astMember symbol_table
  }

(*
  Util functions
*)

let make_default_ctor name =
    Constructor(ClassType(name), new symbol_table, [Public], [],
                [SuperStatement([])])

(*
  "toString" functions for printing an AST although big ASTs are hard to read
*)

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

let strUnOp = function
  | UnPlus -> "un+"
  | UnMinus -> "un-"
  | Not -> "!"

let strModifier = function
  | Static -> "static"
  | Public -> "public"
  | Private -> "private"
  | Protected -> "protected"

let rec strType t =
  let strAppend a x =
    a ^ strType x ^ "; " in
  match t with
  | ClassType s -> "classT(" ^ s ^ ")"
  | ArrayType (ty, n) -> "arrayT(" ^ strType ty ^ ", " ^ string_of_int n ^ ")"
  | BoolType -> "boolT"
  | CharType -> "charT"
  | IntType -> "intT"
  | VoidType -> "voidT"
  | MethodType (c, pList, v) -> "methodT(" ^ strType c ^ ", " ^ List.fold_left strAppend "" pList ^ ", " ^ strType v ^ ")"

let strLiteral = function
  | NullLiteral -> "null"
  | BoolLiteral b -> string_of_bool b
  | IntLiteral i -> string_of_int i
  | CharLiteral c -> String.make 1 c
  | StringLiteral s -> s

let strFormal (f : astFormal) =
  "formal(" ^ f.name ^ ", " ^ strType f.t ^ ")"

let rec strExpr = function
  | UnOpExpr (op, e) -> "unOpExpr(" ^ strUnOp op ^ ", " ^ strExpr e ^ ")"
  | BinOpExpr (op, a, b) -> "binOpExpr(" ^ strBinOp op ^ ", " ^ strExpr a ^ ", " ^ strExpr a ^ ")"
  | PrimaryExpr p -> "primaryExpr(" ^ strPrimary p ^ ")"

and strNewArrayExpr (n : astNewArrayExpr) =
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
    | Some e -> "varDecl(" ^ v.name ^ ", " ^ string_of_int v.dim ^ ", " ^ strExpr e ^ ")"
    | None -> "varDecl(" ^ v.name ^ ", " ^ string_of_int v.dim ^ ")"

let rec strStatement s =
  let strAppend f a x =
    a ^  f x ^ "; " in
  let strAppendStatement = strAppend strStatement in
  let strAppendExpr = strAppend strExpr in
  let strAppendVarDecl = strAppend strVarDecl in

  match s with
  | EmptyStatement -> "emptyStatement"
  | DeclStatement (t, vList) -> "declStatement(" ^ strType t ^ ", " ^ List.fold_left strAppendVarDecl "" vList ^ ")"
  | IfStatement (_, _, e, sA, sO) -> "ifStatement(" ^ strExpr e ^ ", " ^ strStatement sA ^ ", " ^
    (match sO with
    | Some sB -> strStatement sB
    | None -> "")
    ^ ")"
  | ExprStatement e -> "exprStatement(" ^ strExpr e ^ ")"
  | WhileStatement (_, e, sW) -> "whileStatement(" ^ strExpr e ^ ", " ^ strStatement sW ^ ")"
  | ReturnStatement eO -> "returnStatement(" ^
    (match eO with
    | Some e -> strExpr e
    | None -> "")
    ^ ")"
  | ContinueStatement -> "continueStatement"
  | BreakStatement -> "breakStatement"
  | BlockStatement (_, sList) -> "blockStatement(" ^ List.fold_left strAppendStatement "" sList ^ ")"
  | SuperStatement eList -> "superStatement(" ^ List.fold_left strAppendExpr "" eList ^ ")"

let strMember m =
  let strAppend f a x =
    a ^ f x ^ "; " in
  let strAppendModifier = strAppend strModifier in
  let strAppendFormal = strAppend strFormal in
  let strAppendStatement = strAppend strStatement in

  match m with
  | Field (mList, t, v) -> "field(" ^ List.fold_left strAppendModifier "" mList ^
    ", " ^ strType t ^ ", " ^ strVarDecl v ^ ")"
  | Method (s, t, _, mList, fList, sList) -> "method(" ^ s ^ "," ^ strType t ^
    "," ^ List.fold_left strAppendModifier "" mList ^ ", " ^
    List.fold_left strAppendFormal "" fList  ^ "," ^
    List.fold_left strAppendStatement "" sList ^ ")"
  | Constructor (t, _, mList, fList, sList) -> "constructor(" ^ strType t ^
    "," ^ List.fold_left strAppendModifier "" mList ^ ", " ^
    List.fold_left strAppendFormal "" fList  ^ "," ^
    List.fold_left strAppendStatement "" sList ^ ")"

(* TODO add printing field/method tables *)
let strClass c =
  match c.super with
  | None ->
    "class(" ^ strType c.t ^ ")"
  | Some s ->
    "class(" ^ strType c.t ^ ", " ^ "super(" ^ strType s.t ^ "))"