# Makefile,v
# Copyright (c) INRIA 2007-2017

WD=$(shell pwd)
TOP=../..
include $(TOP)/config/Makefile

DESTDIR=
RM=rm

PACKAGES := camlp5,fmt,hashcons
export IMPORT_OCAMLCFLAGS += -ppopt -pa_passthru-debug 
export IMPORT_OCAMLCFLAGS += -ppopt -pa_import-I -ppopt .


ML=\
	lam.ml lam_hashcons.ml lam_migrate.ml q_ast_lam.ml pa_lam.ml \

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
	$(NOT_OCAMLFIND) reinstall-if-diff lam_example -destdir $(DESTDIR)/lib META $(CMO) $(CMX) $(MLI:.mli=.cmi) $(KITS:.cmo=.o) $(CMI) $(wildcard *.cmt*)
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
	$(OCAMLFIND) ocamldep -package $(PACKAGES),camlp5.phony_quotations -syntax $(SYNTAX) lam.ml q_ast_lam.ml >> .depend.NEW
	$(OCAMLFIND) ocamldep -package $(PACKAGES),camlp5.extend,camlp5.phony_quotations -syntax camlp5r pa_lam.ml >> .depend.NEW
	mv .depend.NEW .depend

-include .depend
