SHELL:=bash

# bash will read this config file first
BASH_ENV:=bash_env.sh
export BASH_ENV

.DEFAULT_GOAL: all

all: 
#	$$ob p1_lib.cmxa fails for some reason
	$$ob p1_examples.native
	echo "make all: finished"

clean: 
	ocamlbuild -clean


