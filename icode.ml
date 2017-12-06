
open Symboltable

let int_sz = 4
let ptr_sz = 4
let char_sz = 1
let bool_sz = 1

let mangle_name classname name is_constructor =
  if is_constructor then
    classname ^ "$$init$"
  else
    classname ^ "$" ^ name

let vtable_name classname =
  "_V$" ^ classname

type classRecord =
  { name: string
  ; super: classRecord option
  ; mutable size: int
  ; field_offset_table: int symbol_table
  ; method_offset_table: int symbol_table
  }