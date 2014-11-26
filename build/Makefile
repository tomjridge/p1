
SRC_DIR:=../src
MLS_TO_EXEC:=p1_examples.ml p1_gen.ml # executables
IGNORE_FOR_LIB:=$(MLS_TO_EXEC) interactive.ml # don't want these in the lib
MLS_FOR_LIB:=$(shell ocamlfind ocamldep -sort *.ml)
MLS_FOR_LIB:=$(filter-out $(IGNORE_FOR_LIB), $(MLS_FOR_LIB)) # p1_examples.ml is an executable
CMO:=$(MLS_FOR_LIB:.ml=.cmo)

LIB:=p1

CAMLCINCLUDES:=str.cma
CAMLOPTINCLUDES:=str.cmxa

# FIXME probably want to look at warning 8 a bit more closely
WARNINGS:=-w -8 #-w -26-8
COMPFLAGS:=$(WARNINGS)

all: 
	make before_all
	make $(CMO) p1.cma p1.cmxa p1_examples.native p1_gen.native

ocamldoc: p1_lib.mli p1_parse_grammar.mli FORCE
	-mkdir ocamldoc
	$(OCAMLDOC) -d ocamldoc -html p1_lib.mli p1_parse_grammar.mli

include Makefile.include

clean:
	rm -f *.ml *.mli *.cmo *.cmi *.cmx *.native *.a *.o *.cma *.cmxa .depend
	rm -rf ocamldoc

FORCE:

