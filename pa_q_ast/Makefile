# Makefile,v
# Copyright (c) INRIA 2007-2017

WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile.sub

DESTDIR=

PACKAGES := $(PACKAGES),pa_ppx.deriving,pa_ppx.deriving_plugins.show,pa_ppx.base,pa_ppx.unmatched_vala
TARGET_PACKAGE=pa_ppx_q_ast

ML=\
	pa_deriving_q_ast.ml pa_ppx_q_ast_runtime.ml \

TOOLSML = mk_meta.ml
ALLSRC=$(ML) $(TOOLSML)
ALLPACKAGES=$(PACKAGES)

all: $(CMO) $(CMX)
	$(MAKE) local-install

doc: $(CMT) $(CMTI)

KITS=$(CMO)

TARGET_INSTALL_FILES=$(FILES_TARGET_INSTALL_FILES)
install:: default_install

local-install::
	$(MAKE) DESTDIR=$(WD)/$(TOP)/local-install/ install

clean::
	rm -rf META mk_meta mk_meta.ml
