# Makefile,v
# Copyright (c) INRIA 2007-2017

include Makefile.include

PACKAGES := $(PACKAGES),fmt

ML=\
	eg_q_ast.ml \

CMO := $(ML:.ml=.cmo)
CMI := $(ML:.ml=.cmi)
CMX := $(ML:.ml=.cmx)
CMT := $(ML:.ml=.cmt)
CMTI := $(MLI:.mli=.cmti)

TESTS=  \

all: $(CMO) $(TESTS)

test: $(TESTS:.byte=)

% : %.opt
	mkdir -p _build && ./$<

eg_q_ast.cmo: eg_q_ast.ml
	$(MAKE) -f Makefile.one PACKAGES="$(PACKAGES),camlp5_example.parser_quotations" $@

.SUFFIXES: .cmo .cmi .ml .mli .cmx .cma .cmxa .cmt .cmti

clean::
	rm -rf META *.cm* *.o

EXTERNAL := $(shell $(OCAMLFIND) query -predicates byte -format '%m' $(PACKAGES) | grep local-install)
$(CMO) $(CMI) $(CMX): $(EXTERNAL)

depend::
	echo "$(CMO) $(CMI) $(CMX): $(EXTERNAL)" > .depend.NEW
	$(OCAMLFIND) ocamldep -package $(PACKAGES),camlp5,camlp5_example.parser_quotations -syntax camlp5o $(ML) >> .depend.NEW \
		&& mv .depend.NEW .depend

-include .depend
