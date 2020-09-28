# Makefile,v
# Copyright (c) INRIA 2007-2017

TESTDIRS = sexp_example eg_sexp_example camlp5_example

all:
	set -e; for i in $(TESTDIRS); do cd $$i; $(MAKE) all; cd ..; done

clean:
	set -e; for i in $(TESTDIRS); do cd $$i; $(MAKE) clean; cd ..; done
