# Makefile,v
# Copyright (c) INRIA 2007-2017

WD=$(shell pwd)
TOP=../../..
include $(TOP)/config/Makefile.sub

DESTDIR=

PACKAGES := camlp5,fmt,hashcons
TARGET_PACKAGE=debruijn_example

export IMPORT_OCAMLCFLAGS += -ppopt -pa_passthru-debug 
export IMPORT_OCAMLCFLAGS += -ppopt -pa_import-I -ppopt .


ML=\
	debruijn.ml debruijn_hashcons.ml debruijn_migrate.ml q_ast_debruijn.ml pa_debruijn.ml \

MLI=
ALLSRC=$(ML) $(MLI)
ALLPACKAGES=$(PACKAGES)

all test: $(CMO) $(CMX)
	$(MAKE) local-install

doc: $(CMT) $(CMTI)

TARGET_INSTALL_FILES=$(FILES_TARGET_INSTALL_FILES)
install:: default_install

local-install::
	$(MAKE) DESTDIR=$(WD)/$(TOP)/local-install/ install

.SUFFIXES: .cmo .cmi .ml .mli .cmx .cma .cmxa .cmt .cmti

clean::
	rm -rf META *.cm* *.o *.a

debruijn_hashcons.cmo: debruijn.cmo debruijn.cmi
debruijn_hashcons.cmx: debruijn.cmx debruijn.cmi
debruijn_migrate.cmo: debruijn_hashcons.cmo debruijn_hashcons.cmi
debruijn_migrate.cmx: debruijn_hashcons.cmx debruijn_hashcons.cmi
q_ast_debruijn.cmo: debruijn_hashcons.cmo debruijn_hashcons.cmi
q_ast_debruijn.cmx: debruijn_hashcons.cmx debruijn_hashcons.cmi
