SHELL=bash

CAMLC=$(shell command -v ocamlc.opt || command -v ocamlc || echo missing_ocamlc)
CAMLOPT=$(shell command -v ocamlopt.opt || command -v ocamlopt || echo missing_ocamlopt)
CAMLCINCLUDES=str.cma -I ../build p1.cma
CAMLOPTINCLUDES=str.cmxa -I ../build p1.cmxa

P1_GEN=../build/p1_gen.native

EXECS=test.ml

all:
	make before_all
	make test.native
	./test.native test.txt

before_all:
	cd ../build && make

test.ml: test.g
	$(P1_GEN) -g $< >$@

$(EXECS:.ml=.native): %.native: %.ml
	$(CAMLOPT) $(COMPFLAGS) $(CAMLOPTINCLUDES) -o $@ $<

clean: 
	rm -f *.{cmi,cmx,native,o}
