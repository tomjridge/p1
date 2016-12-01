# bash ----------------------------------------

SHELL:=bash
# bash will read this config file first
BASH_ENV:=bash_env.sh
export BASH_ENV


# include ----------------------------------------

.DEFAULT_GOAL:=all
include depend.mk

# targets ----------------------------------------

all: symlink FORCE
	$(MAKE) $(naive_xs) $(core_xs) $(extra_xs) $(lib_xs) p1.cma p1.cmxa
	$(MAKE) $$natives
	echo "make all: finished"

%.native: %.ml p1.cmxa
	$$ocamlopt -linkpkg -o $*.native p1.cmxa $*.ml


symlink: 
	ln -s */*.ml */*.mli .
	touch $@

%.x: %.mli %.ml 
	$$ocamlc -c $*.mli $*.ml
	$$ocamlopt -c $*.mli $*.ml
	touch $@

%.x: %.ml
	$$ocamlc -c $*.ml
	$$ocamlopt -c $*.ml
	touch $@

p1.cma: $(xs)
	$$mk_cma -g -a -o $@ $(xs:.x=.cmo)

p1.cmxa: $(xs)
	$$mk_cmxa -g -a -o $@ $(xs:.x=.cmx)

clean: 
	rm -f *.cmi *.cmo *.cmx *.o *.x *.a *.cma *.cmxa

real_clean: clean
	rm -f symlink
	for f in */*.ml */*.mli; do rm -f `basename $$f`; done

FORCE:



# ----------------------------------------

# p1_examples: p1.cmxa
# 	$$ocamlopt -o $@ -linkpkg p1.cmxa p1_examples.ml
# 
# test: $(test_xs)

