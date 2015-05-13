all: 
	cd build && $(MAKE)
	cd gen && $(MAKE)

install: all
	ocamlfind install p1 META build/*.cmi  build/p1.cma build/p1.cmxa build/p1.a

install-bin:
	echo FIXME # executables need to go somewhere

doc: FORCE
	cd build && $(MAKE) && $(MAKE) ocamldoc
	cd doc && $(MAKE)

clean: 
	cd build && $(MAKE) clean
	cd doc && $(MAKE) clean
	cd gen && $(MAKE) clean


FORCE:
