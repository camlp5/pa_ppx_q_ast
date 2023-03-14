# Makefile,v
# Copyright (c) INRIA 2007-2017

WD=$(shell pwd)
TOP=../../..
include $(TOP)/config/Makefile.sub

DESTDIR=

PACKAGES := camlp5,fmt,pa_ppx_q_ast
TARGET_PACKAGE=sexp_menhir_example
export IMPORT_OCAMLCFLAGS += -ppopt -pa_passthru-debug \
	-ppopt -pa_import-I -ppopt . \
	 -ppopt -pa_import-package -ppopt $(PACKAGES)


ML= sexp.ml parser.ml lexer.ml
MLI= parser.mli lexer.mli

ALLSRC=$(ML) $(MLI)
ALLPACKAGES=$(PACKAGES)

test: all

all: $(CMO) $(CMX)
	$(MAKE) local-install

doc: $(CMT) $(CMTI)

parser.ml: parser.mly
	menhir --infer-write-query parser_mock.ml $<
	perl -p -e 's,# \d+.*,,' parser_mock.ml > parser_mock_nolines.ml
	ocamlfind ocamlc -package camlp5 -c parser_mock_nolines.ml
	ocamlfind ocamlc -package camlp5 -i parser_mock.ml > parser_mock.mli
	menhir --explain --infer-read-reply parser_mock.mli $<

lexer.ml: lexer.mll
	ocamllex lexer.mll

TARGET_INSTALL_FILES=$(FILES_TARGET_INSTALL_FILES)
install:: default_install

local-install::
	$(MAKE) DESTDIR=$(WD)/$(TOP)/local-install/ install

.SUFFIXES: .cmo .cmi .ml .mli .cmx .cma .cmxa .cmt .cmti

clean::
	rm -rf META *.cm* *.o *.a
