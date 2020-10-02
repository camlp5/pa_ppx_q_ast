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
PACKAGES := camlp5,fmt,hashcons
SYNTAX = camlp5o
IMPORT_OCAMLCFLAGS += -ppopt -pa_passthru-debug 
IMPORT_OCAMLCFLAGS += -ppopt -pa_import-I -ppopt .


ML=\
	debruijn.ml debruijn_hashcons.ml debruijn_migrate.ml q_ast_debruijn.ml pa_debruijn.ml \

MLI=
CMO := $(ML:.ml=.cmo)
CMI := $(ML:.ml=.cmi)
CMX := $(ML:.ml=.cmx)
CMT := $(ML:.ml=.cmt)
CMTI := $(MLI:.mli=.cmti)

all test: $(CMO) $(CMX) local-install

debruijn_migrate.cmo: debruijn_migrate.ml debruijn.cmi
	$(NOT_OCAMLFIND) preprocess $(IMPORT_OCAMLCFLAGS) -package $(PACKAGES),camlp5.parser_quotations,camlp5.pr_o,pa_ppx.import,pa_ppx_hashcons,pa_ppx_migrate -syntax camlp5o $< > $@.ppo.ml
	$(OCAMLFIND) ocamlc $(IMPORT_OCAMLCFLAGS) -package $(PACKAGES),camlp5.parser_quotations,pa_ppx.import,pa_ppx_hashcons,pa_ppx_migrate -syntax camlp5o -c $@.ppo.ml
	$(OCAMLFIND) ocamlc $(IMPORT_OCAMLCFLAGS) -package $(PACKAGES),camlp5.parser_quotations,pa_ppx.import,pa_ppx_hashcons,pa_ppx_migrate -syntax camlp5o -c $<

debruijn_migrate.cmx: debruijn_migrate.ml debruijn.cmi
	$(OCAMLFIND) ocamlopt $(IMPORT_OCAMLCFLAGS) -package $(PACKAGES),pa_ppx_q_ast,pa_ppx.import,pa_ppx_hashcons,pa_ppx_migrate -syntax camlp5o -c $<

debruijn_hashcons.cmo: debruijn_hashcons.ml debruijn.cmi
	$(NOT_OCAMLFIND) preprocess $(IMPORT_OCAMLCFLAGS) -package $(PACKAGES),camlp5.parser_quotations,camlp5.pr_o,pa_ppx.import,pa_ppx_hashcons -syntax camlp5o $< > $@.ppo.ml
	$(OCAMLFIND) ocamlc $(IMPORT_OCAMLCFLAGS) -package $(PACKAGES),camlp5.parser_quotations,pa_ppx.import,pa_ppx_hashcons -syntax camlp5o -c $@.ppo.ml
	$(OCAMLFIND) ocamlc $(IMPORT_OCAMLCFLAGS) -package $(PACKAGES),camlp5.parser_quotations,pa_ppx.import,pa_ppx_hashcons -syntax camlp5o -c $<

debruijn_hashcons.cmx: debruijn_hashcons.ml debruijn.cmi
	$(OCAMLFIND) ocamlopt $(IMPORT_OCAMLCFLAGS) -package $(PACKAGES),pa_ppx_q_ast,pa_ppx.import,pa_ppx_hashcons -syntax camlp5o -c $<

pa_debruijn.cmo: pa_debruijn.ml
	$(NOT_OCAMLFIND) preprocess $(OCAMLCFLAGS) -package $(PACKAGES),camlp5.parser_quotations,camlp5.extend_m,camlp5.pr_r -syntax camlp5r $< > $@.ppo.ml
	$(OCAMLFIND) ocamlc $(OCAMLCFLAGS) -package $(PACKAGES),camlp5.parser_quotations,camlp5.extend_m -syntax camlp5r -c $@.ppo.ml
	$(OCAMLFIND) ocamlc $(OCAMLCFLAGS) -package $(PACKAGES),camlp5.parser_quotations,camlp5.extend_m -syntax camlp5r -c $<

pa_debruijn.cmx: pa_debruijn.ml
	$(OCAMLFIND) ocamlopt $(OCAMLCFLAGS) -package $(PACKAGES),camlp5.parser_quotations,camlp5.extend_m -syntax camlp5r -c $<

q_ast_debruijn.cmo: q_ast_debruijn.ml
	$(NOT_OCAMLFIND) preprocess $(IMPORT_OCAMLCFLAGS) -package $(PACKAGES),pa_ppx_q_ast,pa_ppx.import,camlp5.pr_o -syntax camlp5o $< > $@.ppo.ml
	$(OCAMLFIND) ocamlc $(IMPORT_OCAMLCFLAGS) -package $(PACKAGES),pa_ppx_q_ast,pa_ppx.import -syntax camlp5o -c $@.ppo.ml
	$(OCAMLFIND) ocamlc $(IMPORT_OCAMLCFLAGS) -package $(PACKAGES),pa_ppx_q_ast,pa_ppx.import -syntax camlp5o -c $<

q_ast_debruijn.cmx: q_ast_debruijn.ml
	$(OCAMLFIND) ocamlopt $(IMPORT_OCAMLCFLAGS) -package $(PACKAGES),pa_ppx_q_ast,pa_ppx.import -syntax camlp5o -c $<

doc: $(CMT) $(CMTI)

KITS=$(CMO)

META: META.pl
	./META.pl > META

install::
	mkdir -p $(DESTDIR)/lib
	./META.pl $(DESTDIR)/lib > META
	$(NOT_OCAMLFIND) reinstall-if-diff debruijn_example -destdir $(DESTDIR)/lib META $(CMO) $(CMX) $(MLI:.mli=.cmi) $(KITS:.cmo=.o) $(CMI) $(wildcard *.cmt*)
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
	$(OCAMLFIND) ocamldep -package $(PACKAGES),camlp5.parser_quotations -syntax $(SYNTAX) debruijn.ml q_ast_debruijn.ml >> .depend.NEW
	$(OCAMLFIND) ocamldep -package $(PACKAGES),camlp5.extend,camlp5.parser_quotations -syntax camlp5r pa_debruijn.ml >> .depend.NEW
	mv .depend.NEW .depend

-include .depend

.ml.cmo:
	$(NOT_OCAMLFIND) preprocess $(OCAMLCFLAGS) -package $(PACKAGES),camlp5.pr_o -syntax $(SYNTAX) $< > $@.ppo.ml
	$(OCAMLFIND) ocamlc $(OCAMLCFLAGS) -package $(PACKAGES) -syntax $(SYNTAX) -c $@.ppo.ml
	$(OCAMLFIND) ocamlc $(OCAMLCFLAGS) -package $(PACKAGES) -syntax $(SYNTAX) -c $<

.ml.cmx:
	$(OCAMLFIND) ocamlopt $(OCAMLCFLAGS) -package $(PACKAGES) -syntax $(SYNTAX) -c $<
