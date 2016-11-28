all: FORCE
	$(MAKE) -C src

install: all
	ocamlfind install p1 META src/*.cmi  src/p1.cma src/p1.cmxa src/p1.a

remove: FORCE
	ocamlfind remove p1

with_ocamlbuild: FORCE
	ocamlbuild -pkg str -I src p1_lib.cmo p1_lib.cmx p1_examples.native test_naive.native

# 
# install-bin:
# 	echo FIXME # executables need to go somewhere
# 
# doc: FORCE
# 	cd build && $(MAKE) && $(MAKE) ocamldoc
# 	cd doc && $(MAKE)

clean: 
	$(MAKE) -C src clean
	ocamlbuild -clean

FORCE:
