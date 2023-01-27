# Makefile,v
# Copyright (c) INRIA 2007-2017

WD=$(shell pwd)
TOP=../..

DESTDIR=
RM=rm

LAUNCH=env TOP=$(TOP) $(TOP)/tools/LAUNCH
OCAMLFIND=$(LAUNCH) ocamlfind
NOT_OCAMLFIND=$(LAUNCH) not-ocamlfind
MKCAMLP5=$(LAUNCH) mkcamlp5
PACKAGES := camlp5,fmt
SYNTAX = camlp5o
export IMPORT_OCAMLCFLAGS += -ppopt -pa_passthru-debug 
export IMPORT_OCAMLCFLAGS += -ppopt -pa_import-I -ppopt .


ML=\
	sexp.ml sexp_hashcons.ml sexp_unique.ml sexp_migrate.ml q_ast_sexp.ml pa_sexp.ml \

MLI=
CMO := $(ML:.ml=.cmo)
CMI := $(ML:.ml=.cmi)
CMX := $(ML:.ml=.cmx)
CMT := $(ML:.ml=.cmt)
CMTI := $(MLI:.mli=.cmti)

all test: $(CMO) $(CMX) local-install

doc: $(CMT) $(CMTI)

KITS=$(CMO)

META: META.pl
	./META.pl > META

install::
	mkdir -p $(DESTDIR)/lib
	./META.pl $(DESTDIR)/lib > META
	$(NOT_OCAMLFIND) reinstall-if-diff sexp_example -destdir $(DESTDIR)/lib META $(CMO) $(CMX) $(MLI:.mli=.cmi) $(KITS:.cmo=.o) $(CMI) $(wildcard *.cmt*)
	$(RM) -f META

local-install::
	$(MAKE) DESTDIR=$(WD)/$(TOP)/local-install/ install

.SUFFIXES: .cmo .cmi .ml .mli .cmx .cma .cmxa .cmt .cmti

clean::
	rm -rf META *.cm* *.o *.a

EXTERNAL := $(shell $(OCAMLFIND) query -predicates byte -format '%m' $(PACKAGES) | grep local-install)
$(CMO) $(CMI) $(CMX): $(EXTERNAL)

depend::
	echo "$(CMO) $(CMI) $(CMX): $(EXTERNAL)" > .depend.NEW
	$(OCAMLFIND) ocamldep -package $(PACKAGES),camlp5.extend,camlp5.parser_quotations -syntax $(SYNTAX) sexp.ml q_ast_sexp.ml >> .depend.NEW
	$(OCAMLFIND) ocamldep -package $(PACKAGES),camlp5.extend,camlp5.parser_quotations -syntax camlp5r pa_sexp.ml >> .depend.NEW
	mv .depend.NEW .depend

-include .depend

YAWRAP=$(TOP)/tools/ya-wrap-ocamlfind

.ml.cmo:
#	$(YAWRAP) $(NOT_OCAMLFIND) preprocess $(OCAMLCFLAGS) -package $(PACKAGES),$(PRPACKAGE) $< > $*_ppo.ml
#	$(OCAMLFIND) ocamlc $(DEBUG) $(WARNERR) $(OCAMLCFLAGS) -package $(PACKAGES) -syntax camlp5o -c $*_ppo.ml
	$(YAWRAP) $(OCAMLFIND) ocamlc $(DEBUG) $(WARNERR) $(OCAMLCFLAGS) -package $(PACKAGES) -c $<

.ml.cmx:
	$(YAWRAP) $(OCAMLFIND) ocamlopt $(DEBUG) $(WARNERR) $(OCAMLCFLAGS) -package $(PACKAGES) -c $<
