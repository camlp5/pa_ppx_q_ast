# Makefile,v
# Copyright (c) INRIA 2007-2017

include Makefile.include
OVERSION = $(shell ocamlc -version)
OVERSION_NAME := $(shell ./make_version_name.sh $(OVERSION))
DIFF=diff -Bwi -u10

PACKAGES := $(PACKAGES),fmt

ML=\
	eg_q_ast.ml \

CMO := $(ML:.ml=.cmo)
CMI := $(ML:.ml=.cmi)
CMX := $(ML:.ml=.cmx)
CMT := $(ML:.ml=.cmt)
CMTI := $(MLI:.mli=.cmti)

TESTS=  expr.quot_r.TEST okexpr.quot_r.TEST \

all test: $(CMO) $(TESTS) $(TESTS:.byte=)


expr.quot_r.TEST:
	cpp -E -DQAST -D$(OVERSION_NAME) quot_r.ml > tmp.quot_r.ml
	$(NOT_OCAMLFIND) preprocess -package camlp5.parser_quotations,camlp5.pr_r \
	-ppopt -l -ppopt 400 -ppopt -sep -ppopt '\n' \
	-syntax camlp5r tmp.quot_r.ml | \
	$(DIFF) -I '(\*' -B quot_r.out.ml -


okexpr.quot_r.TEST:
	cpp -E -DQAST -D$(OVERSION_NAME) quot_r.ml | perl -p -e 's,<:,<:ok,' > tmp.quot_r.ml
	$(NOT_OCAMLFIND) preprocess -package camlp5_example.parser_quotations,camlp5.pr_r \
	-ppopt -l -ppopt 400 -ppopt -sep -ppopt '\n' \
	-syntax camlp5r tmp.quot_r.ml | perl -p -e 's,Camlp5_hashcons.OK,MLast,g' | \
	$(DIFF) -I '(\*' -B quot_r.out.ml -


% : %.opt
	mkdir -p _build && ./$<

eg_q_ast.cmo: eg_q_ast.ml
	$(MAKE) -f Makefile.one PACKAGES="$(PACKAGES),camlp5_example.parser_quotations" $@

.SUFFIXES: .cmo .cmi .ml .mli .cmx .cma .cmxa .cmt .cmti

clean::
	rm -rf META *.cm* *.o tmp*

EXTERNAL := $(shell $(OCAMLFIND) query -predicates byte -format '%m' $(PACKAGES) | grep local-install)
$(CMO) $(CMI) $(CMX): $(EXTERNAL)

depend::
	echo "$(CMO) $(CMI) $(CMX): $(EXTERNAL)" > .depend.NEW
	$(OCAMLFIND) ocamldep -package $(PACKAGES),camlp5,camlp5_example.parser_quotations -syntax camlp5o $(ML) >> .depend.NEW \
		&& mv .depend.NEW .depend

-include .depend
