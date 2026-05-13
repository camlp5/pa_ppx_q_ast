# Makefile,v
# Copyright (c) INRIA 2007-2017

WD=$(shell pwd)
TOP=../../..
include $(TOP)/config/Makefile.sub

DESTDIR=

ML=\
	types.ml \

TESTS=\
	test1.TEST test1b.TEST test1c.TEST \
	test2.TEST \
	test3.TEST \
	test4.TEST test4b.TEST \
	test5.TEST test5b.TEST \
	test6.TEST \
	test7.TEST \
	test8.TEST \
	test9.TEST \
	test10.TEST \
	test11.TEST \
	test12.TEST \
	test13.TEST \
	test14.TEST \
	test15.TEST \

$(TESTS:.TEST=.ml): types.cmo

all test: types.cmo $(TESTS)

#.SECONDARY:

%.TEST: %.r_out fixtures/%.r_out
	$(DIFF) -I '#' -Bwiu fixtures/$*.r_out $*.r_out

QUOTPACKAGES=pa_ppx_q_ast_quotation_test,pa_ppx_q_ast_test_cleanup
#QUOTPACKAGES=pa_ppx_q_ast_quotation_test
#QUOTPACKAGES=
RPPFLAGS=-ppopt -l -ppopt 400 -ppopt -flag -ppopt D -ppopt -sep -ppopt '\n'
OPPFLAGS=-ppopt -l -ppopt 400 -ppopt -flag -ppopt M -ppopt -sep -ppopt '\n'

.ml.r_out:
	$(LAUNCH) $(YAWRAP) $(NOT_OCAMLFIND) preprocess $(OCAMLCFLAGS) -package $(QUOTPACKAGES),camlp5.pr_r -syntax camlp5o $(RPPFLAGS) $< > $@

fixtures/test15.r_out: fixtures/test15.OCAMLVERSIONED.r_out
	cppo -V OCAML:$(ocamlVERSION) $< > $@.NEW && mv $@.NEW $@

.SUFFIXES: .cmo .cmi .ml .mli .cmx .cma .cmxa .cmt .cmti .r_out .TEST

.SECONDARY: types.ml test15.ml

clean::
	rm -rf META *.cm* *.o *.*out types.ml test15.ml fixtures/test15.r_out
