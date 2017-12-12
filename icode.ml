
open Ast
open Symboltable

(* let int_sz = 4
let ptr_sz = 4
let char_sz = 1
let bool_sz = 1 *)

let data_sz = 4

let main_method_name = "_$DecafMain"

let mangle_name classname name is_constructor =
  if is_constructor then
    classname ^ "$$init$"
  else
    classname ^ "$" ^ name

let vtable_name classname =
  "_V$" ^ classname

type icRegister =
    Eax
  | Ebx
  | Ecx
  | Edx
  | Esi
  | Edi
  | Ebp
  | Esp
  | InvalidReg

type classRecord =
  { name: string
  ; super: classRecord option
  ; mutable size: int
  ; field_offset_table: int symbol_table
  ; method_offset_table: int symbol_table
  }

type icCond =
    Greater
  | Less
  | Equals
  | Geq
  | Leq
  | Neq

type icBinOp =
    Move
  | Add
  | Sub
  | Mult
  | Div
  | And
  | Or
  | Mod
  | Xor

type icUnOp =
    Pos
  | Neg
  | Not

type icLiteral =
    IntLiteral of int
  | CharLiteral of char
  | BoolLiteral of bool
  | NullLiteral

type icLoc =
  | LiteralVal of icLiteral
  | LocalVal of int (* offset, NOT id*)
  | IdVal of string
  (* TODO verbatimval isn't necessary if you make all IdVals
  intended to be verbatim by turning ids into offsets before code generation *)
  | VerbatimVal of string
  | RegisterVal of icRegister

type icStatement =
  | BinStatement of icBinOp * icLoc * icLoc
  | UnStatement of icUnOp * icLoc
  | ArrayStatement of icLoc * icLoc * icLoc
  | NewArrayStatement of icLoc * astType * icLoc list
  | NewObjStatement of icLoc * classRecord  * icLoc list
  | MethodCallStatement of icLoc * icLoc * int * icLoc list
  | StaticMethodCallStatement of icLoc * icLoc * icLoc list
  | FieldAccessStatement of icLoc * icLoc * int
  | IfStatement of int symbol_table * icLoc * icStatement list * icStatement list
  | WhileStatement of int symbol_table * icLoc * icStatement list
  | ReturnStatement of icLoc option
  | ContinueStatement
  | BreakStatement
  | SuperStatement of icLoc list

type icMethod =
  { name: string
  ; c: classRecord
  ; mutable size: int
  ; local_offset_table: int symbol_table
  ; mutable statements: icStatement list
  ; static: bool
  }