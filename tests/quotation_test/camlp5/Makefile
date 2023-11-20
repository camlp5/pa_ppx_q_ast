# Makefile,v
# Copyright (c) INRIA 2007-2017

WD=$(shell pwd)
TOP=../../..
include $(TOP)/config/Makefile.sub

DESTDIR=

ML=\

all test: \
	test1.r_out test1.o_out \

PACKAGES=pa_ppx_q_ast_quotation_test,pa_ppx_q_ast_test_cleanup
RPPFLAGS=-ppopt -l -ppopt 400 -ppopt -flag -ppopt D -ppopt -sep -ppopt '\n'
OPPFLAGS=-ppopt -l -ppopt 400 -ppopt -flag -ppopt M -ppopt -sep -ppopt '\n'

.ml.r_out:
	$(LAUNCH) $(YAWRAP) $(NOT_OCAMLFIND) preprocess $(OCAMLCFLAGS) -package $(PACKAGES),camlp5.pr_r -syntax camlp5o $(RPPFLAGS) $< > $@

.ml.o_out:
	$(LAUNCH) $(YAWRAP) $(NOT_OCAMLFIND) preprocess $(OCAMLCFLAGS) -package $(PACKAGES),camlp5.pr_o -syntax camlp5o $(OPPFLAGS) $< > $@

.SUFFIXES: .cmo .cmi .ml .mli .cmx .cma .cmxa .cmt .cmti .r_out .o_out

clean::
	rm -rf META *.cm* *.o *.*out
