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

TESTS= lamh.byte lamh.opt \

all: $(CMO) $(TESTS)

test: $(TESTS:.byte=)

% : %.opt
	mkdir -p _build && ./$<

lamh.byte: lamh.cmo
	$(LAUNCH) $(OCAMLFIND) ocamlc $(DEBUG) $(WARNERR) -package $(PACKAGES),debruijn_example.runtime \
	-linkpkg -linkall $^ -o $@

lamh.opt: lamh.cmx
	$(LAUNCH) $(OCAMLFIND) ocamlopt $(DEBUG) $(WARNERR) -package $(PACKAGES),debruijn_example.runtime \
	-linkpkg -linkall $^ -o $@

.SUFFIXES: .cmo .cmi .ml .mli .cmx .cma .cmxa .cmt .cmti

clean::
	rm -rf META *.cm* *.o *.opt *.byte
