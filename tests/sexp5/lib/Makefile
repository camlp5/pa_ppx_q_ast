# Makefile,v
# Copyright (c) INRIA 2007-2017

WD=$(shell pwd)
TOP=../../..
include $(TOP)/config/Makefile.sub

DESTDIR=

PACKAGES := camlp5,fmt,pa_ppx_q_ast
TARGET_PACKAGE=sexp5_example

ML=\
	sexp.ml sexp_migrate.ml q_ast_sexp.ml pa_sexp.ml \

MLI=
ALLSRC=$(ML) $(MLI)
ALLPACKAGES=$(PACKAGES)

test: all

all: $(CMO) $(CMX)
	$(MAKE) local-install

doc: $(CMT) $(CMTI)

TARGET_INSTALL_FILES=$(FILES_TARGET_INSTALL_FILES)
install:: default_install

local-install::
	$(MAKE) DESTDIR=$(WD)/$(TOP)/local-install/ install

.SUFFIXES: .cmo .cmi .ml .mli .cmx .cma .cmxa .cmt .cmti

clean::
	rm -rf META *.cm* *.o *.a
