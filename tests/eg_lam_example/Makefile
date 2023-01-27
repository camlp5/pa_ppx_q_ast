# Makefile,v
# Copyright (c) INRIA 2007-2017

include Makefile.include

PACKAGES := $(PACKAGES),fmt,pa_ppx.utils

ML=\
	eg_parsing.ml eg_q_ast.ml \

CMO := $(ML:.ml=.cmo)
CMI := $(ML:.ml=.cmi)
CMX := $(ML:.ml=.cmx)
CMT := $(ML:.ml=.cmt)
CMTI := $(MLI:.mli=.cmti)

all test: $(CMO) $(CMX)

doc: $(CMT) $(CMTI)

.SUFFIXES: .cmo .cmi .ml .mli .cmx .cma .cmxa .cmt .cmti

clean::
	rm -rf META *.cm* *.o

EXTERNAL := $(shell $(OCAMLFIND) query -predicates byte -format '%m' $(PACKAGES) | grep local-install)
$(CMO) $(CMI) $(CMX): $(EXTERNAL)

depend::
	echo "$(CMO) $(CMI) $(CMX): $(EXTERNAL)" > .depend.NEW
	$(OCAMLFIND) ocamldep -package $(PACKAGES),camlp5,lam_example.parser_quotations -syntax camlp5o $(ML) >> .depend.NEW \
		&& mv .depend.NEW .depend

-include .depend

YAWRAP=$(TOP)/tools/ya-wrap-ocamlfind

.ml.cmo:
#	$(YAWRAP) $(NOT_OCAMLFIND) preprocess $(OCAMLCFLAGS) -package $(PACKAGES),$(PRPACKAGE) $< > $*_ppo.ml
#	$(OCAMLFIND) ocamlc $(DEBUG) $(WARNERR) $(OCAMLCFLAGS) -package $(PACKAGES) -syntax camlp5o -c $*_ppo.ml
	$(YAWRAP) $(OCAMLFIND) ocamlc $(DEBUG) $(WARNERR) $(OCAMLCFLAGS) -package $(PACKAGES) -c $<

.ml.cmx:
	$(YAWRAP) $(OCAMLFIND) ocamlopt $(DEBUG) $(WARNERR) $(OCAMLCFLAGS) -package $(PACKAGES) -c $<
