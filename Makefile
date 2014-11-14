all: 
	cd build && make

clean: 
	cd build && make clean
	cd doc && make clean
	cd gen && make clean

doc: FORCE
	cd build && make && make ocamldoc
	cd doc && make

FORCE:
