all: FORCE
	$(MAKE) -C src

install: all
	ocamlfind install p1 META `find src -name "*.cmi" -o -name "*.cma" -o -name "*.cmxa" -o -name "*.a"`

uninstall: FORCE
	ocamlfind remove p1

clean: 
	$(MAKE) -C src clean

FORCE:

