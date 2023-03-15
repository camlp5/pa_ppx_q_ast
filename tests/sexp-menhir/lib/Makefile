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


ML= sexp.ml sexp_parser.ml sexp_lexer.ml sexp_parse.ml
MLI= sexp_parser.mli sexp_lexer.mli

ALLSRC=$(ML) $(MLI)
ALLPACKAGES=$(PACKAGES)

test: all

all: $(CMO) $(CMX)
	$(MAKE) local-install

doc: $(CMT) $(CMTI)

sexp_parser.ml: sexp_parser.mly
	menhir --infer-write-query sexp_parser_mock.ml $<
	perl -p -e 's,# \d+.*,,' sexp_parser_mock.ml > sexp_parser_mock_nolines.ml
	ocamlfind ocamlc -package camlp5 -c sexp_parser_mock_nolines.ml
	ocamlfind ocamlc -package camlp5 -i sexp_parser_mock.ml > sexp_parser_mock.mli
	menhir --explain --infer-read-reply sexp_parser_mock.mli $<

sexp_lexer.ml: sexp_lexer.mll
	ocamllex sexp_lexer.mll

TARGET_INSTALL_FILES=$(FILES_TARGET_INSTALL_FILES)
install:: default_install

local-install::
	$(MAKE) DESTDIR=$(WD)/$(TOP)/local-install/ install

.SUFFIXES: .cmo .cmi .ml .mli .cmx .cma .cmxa .cmt .cmti

clean::
	rm -rf META *.cm* *.o *.a
