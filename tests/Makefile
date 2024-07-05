# Makefile,v
# Copyright (c) INRIA 2007-2017

WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile.sub

TESTDIRS = \
	quotation_test/simple \
	quotation_test/camlp5 \
	basic/lib basic/test \
	basic2/lib basic2/test \
	sexp/lib sexp/test \
	lam/lib lam/test \
	debruijn/lib debruijn/test \
	camlp5/lib camlp5/test \
	sexp2/lib sexp2/test \
	sexp3/lib sexp3/test \
	sexp4/lib sexp4/test \
	sexp5/lib sexp5/test \
	sexp-menhir/lib \
	sexp-menhir/test \
	quotation_test/sexp-menhir \
	quotation_test/parsetree \


test:
	set -e; for i in $(TESTDIRS); do cd $$i; $(MAKE) test; cd ../..; done

all:
	set -e; for i in $(TESTDIRS); do cd $$i; $(MAKE) all; cd ../..; done

clean::
	set -e; for i in $(TESTDIRS); do cd $$i; $(MAKE) clean; cd ../..; done

depend::
	set -e; for i in $(TESTDIRS); do cd $$i; $(MAKE) depend; cd ../..; done
