SHELL:=bash

# bash will read this config file first
BASH_ENV:=bash_env.sh
export BASH_ENV

.DEFAULT_GOAL: all

include depend.mk

all: symlink FORCE
	$(MAKE) $(naive_xs) $(core_xs) $(extra_xs) $(lib_xs) $(examples_xs)
	echo "make all: finished"

symlink: 
	ln -s */*.ml */*.mli .
	touch $@

#	assert_in_build


%.x: %.mli %.ml 
	$$ocamlc -c $*.mli $*.ml
	$$ocamlopt -c $*.mli $*.ml
	touch $@

%.x: %.ml
	$$ocamlc -c $*.ml
	$$ocamlopt -c $*.ml
	touch $@

p1_lib.cma: $(xs)
	$$mk_cma -g -a -o $@ $(xs:.x=.cmo)

p1_lib.cmxa: $(xs)
	$$mk_cmxa -g -a -o $@ $(xs:.x=.cmx)

clean: 
	rm -f *.cmi *.cmo *.cmx *.o *.x

real_clean: clean
	rm -f symlink *.ml *.mli 

FORCE:


# CAMLCINCLUDES:=-package str
# CAMLOPTINCLUDES:=-package str
# COMPFLAGS:=-w -8 -w -23 -bin-annot
# 
# MLS:=$(filter-out interactive.ml, $(shell ocamlfind ocamldep -sort *.ml *.mli))
# SRC_DIR:=../src
# 
# MLS_TO_EXEC:=p1_examples.ml p1_gen.ml p1_gen_p4.ml # executables
# IGNORE_FOR_LIB:=$(MLS_TO_EXEC) interactive.ml # don't want these in the lib
# MLS_FOR_LIB:=$(filter-out $(IGNORE_FOR_LIB), $(MLS)) # p1_examples.ml is an executable
# LIB:=p1
# 
# 
# all: before_all
# 	$(MAKE) cmo cmx $(LIB).cma $(LIB).cmxa $(MLS_TO_EXEC:.ml=.native)
# 
# 
# include Makefile.include
# 
# ocamldoc: p1_lib.mli p1_parse_grammar.mli FORCE
# 	-mkdir ocamldoc
# 	$(OCAMLDOC) -d ocamldoc -html p1_lib.mli p1_parse_grammar.mli
# 
# clean: before_clean
# 	rm -f pending.ml.todo
# 
