all: FORCE
	$(MAKE) -C src

install: all
	ocamlfind install p1 META src/*.cmi  src/p1.cma src/p1.cmxa src/p1.a

# 
# install-bin:
# 	echo FIXME # executables need to go somewhere
# 
# doc: FORCE
# 	cd build && $(MAKE) && $(MAKE) ocamldoc
# 	cd doc && $(MAKE)

clean: 
	$(MAKE) -C src clean

FORCE:
