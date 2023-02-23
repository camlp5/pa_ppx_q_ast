# Makefile,v
# Copyright (c) INRIA 2007-2017

WD=$(shell pwd)
TOP=../../..
include $(TOP)/config/Makefile.sub

DESTDIR=

PACKAGES := camlp5,fmt,hashcons
TARGET_PACKAGE=lam_example

export IMPORT_OCAMLCFLAGS += -ppopt -pa_passthru-debug 
export IMPORT_OCAMLCFLAGS += -ppopt -pa_import-I -ppopt .


ML=\
	lam.ml lam_hashcons.ml lam_migrate.ml q_ast_lam.ml pa_lam.ml \

MLI=
ALLSRC=$(ML) $(MLI)
ALLPACKAGES=$(PACKAGES)

all test: $(CMO) $(CMX)
	$(MAKE) local-install

doc: $(CMT) $(CMTI)

KITS=$(CMO)

TARGET_INSTALL_FILES=$(FILES_TARGET_INSTALL_FILES)
install:: default_install

local-install::
	$(MAKE) DESTDIR=$(WD)/$(TOP)/local-install/ install

.SUFFIXES: .cmo .cmi .ml .mli .cmx .cma .cmxa .cmt .cmti

clean::
	rm -rf META *.cm* *.o *.a

lam_hashcons.cmo: lam.cmo lam.cmi
lam_hashcons.cmx: lam.cmx lam.cmi
lam_migrate.cmo: lam_hashcons.cmo lam_hashcons.cmi
lam_migrate.cmx: lam_hashcons.cmx lam_hashcons.cmi
q_ast_lam.cmo: lam_hashcons.cmo lam_hashcons.cmi
q_ast_lam.cmx: lam_hashcons.cmx lam_hashcons.cmi
