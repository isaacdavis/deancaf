
open Ast
open Symboltable
open Typechecker

let object_class =
  { t = ClassType("Object")
  ; super = None
  ; constructor = make_default_ctor "Object"
  ; fieldTable = new symbol_table
  ; methodTable = new symbol_table
  }

let setup () =
  class_table#put "Object" object_class