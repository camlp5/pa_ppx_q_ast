# Makefile,v
# Copyright (c) INRIA 2007-2017

TESTDIRS = sexp_example eg_sexp_example \
	lam_example eg_lam_example \
	debruijn_example eg_debruijn_example \
	 camlp5_example eg_camlp5_example

test: all
	set -e; for i in $(TESTDIRS); do cd $$i; $(MAKE) test; cd ..; done

all:
	set -e; for i in $(TESTDIRS); do cd $$i; $(MAKE) all; cd ..; done

clean:
	set -e; for i in $(TESTDIRS); do cd $$i; $(MAKE) clean; cd ..; done

depend:
	set -e; for i in $(TESTDIRS); do cd $$i; $(MAKE) depend; cd ..; done
