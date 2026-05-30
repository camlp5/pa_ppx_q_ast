# Makefile,v
# Copyright (c) INRIA 2007-2017

WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile.sub

QUOTTESTDIRS = \
	quotation_test/simple \
	quotation_test/camlp5 \
	sexp-menhir/lib \
	quotation_test/sexp-menhir \
	quotation_test/parsetree \
	camlp5/lib camlp5/test \

TESTDIRS = \
	$(QUOTTESTDIRS) \
	basic/lib basic/test \
	basic2/lib basic2/test \
	basic3/lib basic3/test \
	sexp/lib sexp/test \
	lam/lib lam/test \
	debruijn/lib debruijn/test \
	sexp2/lib sexp2/test \
	sexp3/lib sexp3/test \
	sexp4/lib sexp4/test \
	sexp5/lib sexp5/test \
	sexp-menhir/lib \
	sexp-menhir/test \


test:
	set -e; for i in $(TESTDIRS); do cd $$i; $(MAKE) test; cd ../..; done

quotation-test:
	set -e; for i in $(QUOTTESTDIRS); do cd $$i; $(MAKE) test; cd ../..; done

all:
	set -e; for i in $(TESTDIRS); do cd $$i; $(MAKE) all; cd ../..; done

clean::
	set -e; for i in $(TESTDIRS); do cd $$i; $(MAKE) clean; cd ../..; done

depend::
	set -e; for i in $(TESTDIRS); do cd $$i; $(MAKE) depend; cd ../..; done
