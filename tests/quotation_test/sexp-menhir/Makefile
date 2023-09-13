# Makefile,v
# Copyright (c) INRIA 2007-2017

WD=$(shell pwd)
TOP=../../..
include $(TOP)/config/Makefile.sub

DESTDIR=

ML=\

all test: test1.TEST

export IMPORT_OCAMLCFLAGS := \
	-ppopt -pa_passthru-debug \
	-ppopt -pa_import-package -ppopt camlp5 \
	-ppopt -pa_import-I -ppopt . \
	-ppopt -pa_import-I -ppopt ../../sexp-menhir/lib \

PACKAGES=pa_ppx_q_ast_quotation_test,pa_ppx_q_ast_test_cleanup
#PACKAGES=pa_ppx_q_ast_quotation_test
RPPFLAGS=-ppopt -l -ppopt 400 -ppopt -flag -ppopt D -ppopt -sep -ppopt '\n'
OPPFLAGS=-ppopt -l -ppopt 400 -ppopt -flag -ppopt M -ppopt -sep -ppopt '\n'

%.TEST: %.patterns_out %.types_out
	$(DIFF) -Bwiu $^

%.types_out: %_types.ml
	$(LAUNCH) $(YAWRAP) $(NOT_OCAMLFIND) preprocess $(OCAMLCFLAGS) -package $(PACKAGES),camlp5.pr_r $(RPPFLAGS) $< > $@

%.patterns_out: %_patterns.ml
	$(LAUNCH) $(YAWRAP) $(NOT_OCAMLFIND) preprocess $(OCAMLCFLAGS) -package $(PACKAGES),camlp5.pr_r $(RPPFLAGS) $< > $@

.SUFFIXES: .cmo .cmi .ml .mli .cmx .cma .cmxa .cmt .cmti .r_out .o_out

clean::
	rm -rf META *.cm* *.o *.*out
