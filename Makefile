all: 
	cd build && $(MAKE)

clean: 
	cd build && $(MAKE) clean
	cd doc && $(MAKE) clean
	cd gen && $(MAKE) clean

doc: FORCE
	cd build && $(MAKE) && $(MAKE) ocamldoc
	cd doc && $(MAKE)

FORCE:
