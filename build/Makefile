SHELL:=bash

CAMLCINCLUDES:=-package str
CAMLOPTINCLUDES:=-package str
COMPFLAGS:=-w -8 -w -23 -bin-annot

MLS:=$(filter-out interactive.ml, $(shell ocamlfind ocamldep -sort *.ml *.mli))
SRC_DIR:=../src

MLS_TO_EXEC:=p1_examples.ml p1_gen.ml p1_gen_p4.ml # executables
IGNORE_FOR_LIB:=$(MLS_TO_EXEC) interactive.ml # don't want these in the lib
MLS_FOR_LIB:=$(filter-out $(IGNORE_FOR_LIB), $(MLS)) # p1_examples.ml is an executable
LIB:=p1


all: before_all
	$(MAKE) cmo cmx $(LIB).cma $(LIB).cmxa $(MLS_TO_EXEC:.ml=.native)


include Makefile.include

ocamldoc: p1_lib.mli p1_parse_grammar.mli FORCE
	-mkdir ocamldoc
	$(OCAMLDOC) -d ocamldoc -html p1_lib.mli p1_parse_grammar.mli

clean: before_clean
	rm -f pending.ml.todo

FORCE:
