
open Ast
open Symboltable

exception TypeError of string

let class_table : astClass symbol_table = new symbol_table
