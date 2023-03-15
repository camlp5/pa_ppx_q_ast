# Makefile,v
# Copyright (c) INRIA 2007-2017


WD=$(shell pwd)
TOP=../../..
include $(TOP)/config/Makefile.sub

DESTDIR=

PACKAGES := $(PACKAGES),fmt

ML=\
	eg_parsing.ml eg_q_ast.ml \

ALLSRC=$(ML) $(MLI)
ALLPACKAGES=$(PACKAGES)

all: $(CMO) $(CMX)

test:: all example.asciidoc.TEST


doc: $(CMT) $(CMTI)

.SUFFIXES: .cmo .cmi .ml .mli .cmx .cma .cmxa .cmt .cmti

clean::
	rm -rf META *.cm* *.o
