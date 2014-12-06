SHELL:=bash

CAMLCINCLUDES:=str.cma
CAMLOPTINCLUDES:=str.cmxa
COMPFLAGS:=-w -8 -w -23

MLS:=$(filter-out interactive.ml, $(shell ocamlfind ocamldep -sort *.ml *.mli))
SRC_DIR:=../src

MLS_TO_EXEC:=p1_examples.ml p1_gen.ml p1_gen_p4.ml # executables
IGNORE_FOR_LIB:=$(MLS_TO_EXEC) interactive.ml # don't want these in the lib
MLS_FOR_LIB:=$(filter-out $(IGNORE_FOR_LIB), $(MLS)) # p1_examples.ml is an executable
LIB:=p1


all: before_all
	$(MAKE) cmo cmx 
	$(MAKE) $(LIB).cma $(LIB).cmxa $(MLS_TO_EXEC:.ml=.native)


include Makefile.include


clean: before_clean
	rm -f pending.ml.todo

