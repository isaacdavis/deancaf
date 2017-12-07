
open Ast
open Symboltable

let int_sz = 4
let ptr_sz = 4
let char_sz = 1
let bool_sz = 1

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

type icUnOp =
    Pos
  | Neg
  | Not

type icLiteral =
    IntLiteral of int
  | CharLiteral of char
  | BoolLiteral of bool
  | NullLiteral

type icVal =
    LiteralVal of icLiteral
  | TemporaryVal of icTemporary

and icTemporary =
  { name: string
  ; v: icVal
  ; reg: icRegister
  }

type icStatement =
    Cond of icCond * icVal * icVal
  | BinStatement of icBinOp * icVal * icVal
  | UnStatement of icUnOp * icVal * icVal
  | ArrayStatement of icVal * astType * int
  | NewArrayStatement of astType * int * int
  | NewObjStatement of classRecord
  | MethodCallStatement of icVal * classRecord * string
  | FieldAccessStatement of icVal * classRecord * string
  | IfStatement of icVal * icStatement list
  | WhileStatement of icVal * icStatement list
  | ReturnStatment of icVal option
  | ContinueStatment
  | BreakStatment

(* type icBlock =
  { statements: icStatement list
  ; next: icBlock option
  }
*)

type icMethod =
  { name: string
  ; c: classRecord
  ; mutable size: int
  ; local_offset_table: int symbol_table
  ; mutable statements: icStatement list
  }
  