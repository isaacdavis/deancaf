# deancaf
A compiler for the Decaf programming language, written in OCaml. Wow!

## The language:
Information about Decaf can be found [here](http://cs.brown.edu/courses/csci1260/).
This compiler outputs a 32-bit x86 Linux executable.

There's a little help from a runtime library written in C, and gcc to link and assemble the binary.

## Dependencies:
Ocaml 4.01.0 or later, including ocamlc, ocamlbuild, ocamllex, and ocamlyacc

## To build:
Run `make decafc`

## To run:

`./decafc <file to be compiled> <output file>` compiles a decaf file and outputs assembly in `<output file.s>` and a binary in `<output file>`

Rad!
