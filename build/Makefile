SHELL=bash

CAMLC=$(shell command -v ocamlc.opt || command -v ocamlc || echo missing_ocamlc)
CAMLOPT=$(shell command -v ocamlopt.opt || command -v ocamlopt || echo missing_ocamlopt)
CAMLDEP=$(shell command -v ocamldep.opt || command -v ocamldep || echo missing_ocamldep) -one-line
OCAMLDOC=$(shell command -v ocamldoc.opt || command -v ocamldoc || echo missing_ocamldoc)
CAMLCINCLUDES=str.cma
CAMLOPTINCLUDES=str.cmxa

# FIXME probably want to look at warning 8 a bit more closely
WARNINGS=-w -8 #-w -26-8
COMPFLAGS=$(WARNINGS)

IGNORE_FOR_LIB:=p1_examples.ml interactive.ml p1_gen.ml # don't want these in the lib

MLS_FOR_LIB:=$(shell $(CAMLDEP) -sort *.ml)
MLS_FOR_LIB:=$(filter-out $(IGNORE_FOR_LIB), $(MLS_FOR_LIB)) # p1_examples.ml is an executable
CMO=$(MLS_FOR_LIB:.ml=.cmo)

EXECS=p1_examples.ml p1_gen.ml # executables

all: 
	make before_all
	make $(CMO) p1.cma p1.cmxa p1_examples.native p1_gen.native

before_all: link depend

link: 
	find ../src \( -name "*.ml" -or -name "*.mli" \) -maxdepth 1 -exec ln -sf \{\} . \;

p1.cma: $(CMO)
	$(CAMLC) $(COMPFLAGS) -a -o $@ $(CMO)

p1.cmxa: $(CMO:.cmo=.cmx)
	$(CAMLOPT) $(COMPFLAGS) -a -o $@ $(CMO:.cmo=.cmx)

$(EXECS:.ml=.native): %.native: %.ml p1.cmxa
	$(CAMLOPT) $(COMPFLAGS) $(CAMLOPTINCLUDES) -o $@ p1.cmxa $<


# Default rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(CAMLC) $(CAMLCINCLUDES) $(COMPFLAGS) -c $<

.mli.cmi:
	$(CAMLC) $(CAMLCINCLUDES) $(COMPFLAGS) -c $<

.ml.cmx:
	$(CAMLOPT) $(COMPFLAGS) $(CAMLOPTINCLUDES) -c $<


ocamldoc: p1_lib.mli p1_parse_grammar.mli FORCE
	-mkdir ocamldoc
	$(OCAMLDOC) -d ocamldoc -html p1_lib.mli p1_parse_grammar.mli


depend:
	$(CAMLDEP) *.mli *.ml >.depend

-include .depend


clean:
	find . ! \( -name Makefile -or -name . -or -name old \) -exec rm -f \{\} \;
	rm -rf ocamldoc

realclean:

FORCE:
