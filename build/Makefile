SHELL=bash

CAMLC=$(shell command -v ocamlc.opt || command -v ocamlc || echo missing_ocamlc)
CAMLOPT=$(shell command -v ocamlopt.opt || command -v ocamlopt || echo missing_ocamlopt)
CAMLDEP=$(shell command -v ocamldep.opt || command -v ocamldep || echo missing_ocamldep) -one-line
OCAMLDOC=$(shell command -v ocamldoc.opt || command -v ocamldoc || echo missing_ocamldoc)

# FIXME probably want to look at warning 8 a bit more closely
WARNINGS=-w -8 #-w -26-8
COMPFLAGS=$(WARNINGS)

P1=p1_prelude.cmo p1_core.cmo p1_parsers.cmo p1_lib.cmo # p1_everything.cmo p1_lib.cmo tr_simple_file.cmo
CMO=$(P1) 

NATIVES=# 

all: p1_lib.cmo #p1.cma p1.cmxa

# p3_$(VERSION).cmo: p3.cma
# 	$(CAMLC) $(CAMLCINCLUDES) -pack -o $@ $(CMO)

# we have to compile with make clean; FORPACK="-for-pack P3_x_x" make p3_x_x.cmx ; then make clean and recompile as normal
p3_$(VERSION).cmx: $(CMO:.cmo=.cmx)
	$(CAMLOPT) $(CAMLOPTINCLUDES) -pack -o $@ $(CMO:.cmo=.cmx)

p1.cma: $(CMO)
	$(CAMLC) $(CAMLCINCLUDES) -a -o $@ $(CMO)

p1.cmxa: $(CMO:.cmo=.cmx)
	$(CAMLOPT) -a -o $@ $(CMO:.cmo=.cmx)

$(NATIVES): %.native: %.ml p3.cmxa
	$(CAMLOPT) $(COMPFLAGS) $(CAMLOPTINCLUDES) -o $@ p3.cmxa $<


# Default rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(CAMLC) $(CAMLCINCLUDES) $(COMPFLAGS) -c $<

.mli.cmi:
	$(CAMLC) $(CAMLCINCLUDES) $(COMPFLAGS) -c $<

.ml.cmx:
	$(CAMLOPT) $(FORPACK) $(COMPFLAGS) $(CAMLOPTINCLUDES) -c $<


doc: $(P3)
	$(OCAMLDOC) -html $(P3:.cmo=.ml)


depend:
	$(CAMLDEP) $(INCLUDES) *.mli *.ml >.depend

include .depend


clean:
	rm -f $(CMO) $(CMO:.cmo=.cmi) $(CMO:.cmo=.cmx) $(NATIVES:.native=.cmo) $(NATIVES:.native=.cmi) $(NATIVES:.native=.cmx) {minip3,mini_p3_lib}.cm[iox]
	rm -f *.cma *.cmxa *.a $(CMO:.cmo=.o) *.native $(NATIVES:.native=.o) {minip3,mini_p3_lib}.o
	rm -f *.{html,css}

realclean:
	rm -f p3_$(VERSION).{o,cmi,cmo,cmx}
