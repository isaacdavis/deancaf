
open Ast
open Symboltable
open Typechecker

(*
  Ast objects for runtime classes
*)

let is_runtime_class s =
  (String.compare s "Object" == 0) ||
  (String.compare s "String" == 0) ||
  (String.compare s "IO" == 0)

let object_class =
  { t = ClassType("Object")
  ; super = None
  ; constructor = make_default_ctor "Object"
  ; fieldTable = new symbol_table
  ; methodTable = new symbol_table
  }

let string_class =
  { t = ClassType("String")
  ; super = Some object_class
  ; constructor = make_default_ctor "String"
  ; fieldTable = new symbol_table
  ; methodTable = new symbol_table
  }

let io_ctor = Constructor(ClassType("IO"), new symbol_table, [Private], [], [])

let io_methods = [
  "putChar", Method("putChar", VoidType, new symbol_table, [Public; Static],
    [{name = ""; t = CharType}], []);
  "putInt", Method("putInt", VoidType, new symbol_table, [Public; Static],
    [{name = ""; t = IntType}], []);
  "putString", Method("putString", VoidType, new symbol_table, [Public; Static], 
    [{name = ""; t = ClassType("String")}], []);
  "peek", Method("peek", IntType, new symbol_table, [Public; Static], [], []);
  "getChar", Method("getChar", IntType, new symbol_table, [Public; Static], [],
    []);
  "getInt", Method("getInt", IntType, new symbol_table, [Public; Static], [],
    []);
  "getLine", Method("getLine", ClassType("String"), new symbol_table,
    [Public; Static], [], [])
]

let io_class =
  { t = ClassType("IO")
  ; super = Some object_class
  ; constructor = io_ctor
  ; fieldTable = new symbol_table
  ; methodTable = new symbol_table
  }

let add_runtime_classes classlist =
  [object_class; string_class; io_class] @ classlist

let setup () =
  List.iter (fun m -> io_class.methodTable#put (fst m) (snd m)) io_methods;

  class_table#put "Object" object_class;
  class_table#put "String" string_class;
  class_table#put "IO" io_class
