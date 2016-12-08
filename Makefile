all:
	$(MAKE) -C src

install: all
	ocamlfind install p1 src/META `find src -name "*.cmi" -o -name "*.cma" -o -name "*.cmxa" -o -name "*.a"`

uninstall: 
	ocamlfind remove p1

clean: 
	$(MAKE) -C src clean real_clean
