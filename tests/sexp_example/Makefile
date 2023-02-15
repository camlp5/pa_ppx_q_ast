# Makefile,v
# Copyright (c) INRIA 2007-2017

WD=$(shell pwd)
TOP=../..
include $(TOP)/config/Makefile.sub

DESTDIR=

PACKAGES := camlp5,fmt
export IMPORT_OCAMLCFLAGS += -ppopt -pa_passthru-debug 
export IMPORT_OCAMLCFLAGS += -ppopt -pa_import-I -ppopt .


ML=\
	sexp.ml sexp_hashcons.ml sexp_unique.ml sexp_migrate.ml q_ast_sexp.ml pa_sexp.ml \

MLI=
ALLSRC=$(ML) $(MLI)
ALLPACKAGES=$(PACKAGES)

all test: $(CMO) $(CMX) local-install

doc: $(CMT) $(CMTI)

KITS=$(CMO)

install:: META
	mkdir -p $(DESTDIR)/lib
	$(NOT_OCAMLFIND) reinstall-if-diff sexp_example -destdir $(DESTDIR)/lib META $(CMO) $(CMX) $(MLI:.mli=.cmi) $(KITS:.cmo=.o) $(CMI) $(wildcard *.cmt*)

local-install::
	$(MAKE) DESTDIR=$(WD)/$(TOP)/local-install/ install

.SUFFIXES: .cmo .cmi .ml .mli .cmx .cma .cmxa .cmt .cmti

clean::
	rm -rf META *.cm* *.o *.a
