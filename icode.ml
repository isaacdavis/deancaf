
open Ast
open Symboltable

(* TODO support differentiated data sizes *)
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

type icClassRecord =
  { name: string
  ; super: icClassRecord option
  ; mutable size: int
  ; field_offset_table: int symbol_table
  ; method_offset_table: int symbol_table
  (* Holds which classes actually declare inherited methods *)
  ; method_inherited_table: string symbol_table
  }

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
  | Lea
  | Greater
  | Less
  | Equals
  | Geq
  | Leq
  | Neq

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
  (* offset, NOT id*)
  | LocalVal of int
  | IdVal of string
  (* TODO verbatimval isn't necessary if you make all IdVals
  intended to be verbatim by turning ids into offsets before code generation *)
  (* Do not even look up string - just write it verbatim*)
  | VerbatimVal of string
  | RegisterVal of icRegister
  (* | ArrayLocationVal of icLoc * astType * icLoc *)
  (* | FieldLocationVal of icLoc * int *)

type icStatement =
  | BinStatement of icBinOp * icLoc * icLoc
  | UnStatement of icUnOp * icLoc
  (* destination, array, index *)
  | ArrayStatement of icLoc * icLoc * icLoc
  (* destination, type, index args *)
  | NewArrayStatement of icLoc * astType * icLoc list
  (* destination, class template, constructor args *)
  | NewObjStatement of icLoc * icClassRecord  * icLoc list
  (* destination, callee, vtable offset, args *)
  | MethodCallStatement of icLoc * icLoc * int * icLoc list
  (* destination, name, args *)
  | StaticMethodCallStatement of icLoc * icLoc * icLoc list
  (* destination, object, field offset *)
  | FieldAccessStatement of icLoc * icLoc * int
  | IfStatement of int symbol_table * icLoc * icStatement list *
    icStatement list
  | WhileStatement of int symbol_table * icLoc * icStatement list
  | ReturnStatement of icLoc option
  | ContinueStatement
  | BreakStatement
  (* args *)
  | SuperStatement of icLoc list

type icMethodFrame =
  { name: string
  ; c: icClassRecord
  ; mutable size: int
  ; local_offset_table: int symbol_table
  ; mutable statements: icStatement list
  ; static: bool
  }