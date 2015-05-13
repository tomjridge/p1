SHELL=bash

CAMLC=ocamlfind ocamlc
CAMLOPT=ocamlfind ocamlopt
CAMLCINCLUDES=-package str -I ../build p1.cma
CAMLOPTINCLUDES=$(CAMLCINCLUDES:.cma=.cmxa)

P1_GEN=../build/p1_gen.native

EXECS=test.ml

all:
	make clean
	make before_all
	make test.native

test:
	./test.native test.txt

before_all:
	cd ../build && make

test.ml: test.g
	$(P1_GEN) -g $< >$@

$(EXECS:.ml=.native): %.native: %.ml
	$(CAMLOPT) $(COMPFLAGS) $(CAMLOPTINCLUDES) -linkpkg -o $@ $<

clean: 
	rm -f *.{cmi,cmx,native,o} test.ml
