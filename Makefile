CC = ocamlbuild
EXECS = decafc.byte
SOURCES = ast.ml codegen.ml decafc.ml icode.ml icodegen.ml runtime.ml \
			symboltable.ml typechecker.ml lexer.mll parser.mly

all: decafc

decafc: $(SOURCES)
	$(CC) $(EXECS)
	ln -s -f $(EXECS) $@

clean:
	rm -rf ./_build $(EXECS) ./decafc

.PHONY: all clean
