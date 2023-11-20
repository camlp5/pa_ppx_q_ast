# Makefile,v
# Copyright (c) INRIA 2007-2017

WD=$(shell pwd)
TOP=../../..
include $(TOP)/config/Makefile.sub

DESTDIR=

OVERSION = $(shell ocamlc -version)
OVERSION_NAME := $(shell ./make_version_name.sh $(OVERSION))
$(info "OVERSION: " $(OVERSION))
$(info "OVERSION_NAME: " $(OVERSION_NAME))
DIFF:=$(DIFF) -Bwi -u10

PACKAGES := $(PACKAGES),camlp5,fmt,pa_ppx.utils,camlp5_example

ML=\
	eg_parsing.ml eg_q_ast.ml \

ALLSRC=$(ML) $(MLI)
ALLPACKAGES=$(PACKAGES)

TESTS=  expr.quot_r.TEST okexpr.quot_r.TEST hcexpr.quot_r.TEST \

all: $(CMO)

test: all
	$(MAKE) $(TESTS)

MKTEST_PACKAGES=pa_ppx_q_ast_quotation_test,pa_ppx_q_ast_test_cleanup
RPPFLAGS=-ppopt -l -ppopt 400 -ppopt -flag -ppopt D -ppopt -sep -ppopt '\n'

quot_r.out.ml: quot.ml
	$(LAUNCH) $(YAWRAP) $(NOT_OCAMLFIND) preprocess $(OCAMLCFLAGS) -package $(MKTEST_PACKAGES),camlp5.pr_r -syntax camlp5o $(RPPFLAGS) $< > $@

expr.quot_r.TEST: quot_r.out.ml
	opam var os
	echo "DIFF: " $(DIFF)
	cpp -E -DQAST -D$(OVERSION_NAME) quot_r.ml > tmp.quot_r.ml
	$(LAUNCH) $(NOT_OCAMLFIND) preprocess -package camlp5.parser_quotations,camlp5.pr_r \
	-ppopt -l -ppopt 400 -ppopt -sep -ppopt '\n' \
	-syntax camlp5r tmp.quot_r.ml | \
	$(DIFF) -I '(\*' -B quot_r.out.ml -


okexpr.quot_r.TEST: quot_r.out.ml
	cpp -E -DQAST -D$(OVERSION_NAME) quot_r.ml | perl -p -e 's,<:,<:ok,' > oktmp.quot_r.ml
	$(LAUNCH) $(NOT_OCAMLFIND) preprocess -package camlp5_example.parser_quotations,camlp5.pr_r \
	-ppopt -l -ppopt 400 -ppopt -sep -ppopt '\n' \
	-syntax camlp5r oktmp.quot_r.ml | perl -p -e 's,Camlp5_hashcons.OK,MLast,g' | \
	$(DIFF) -I '(\*' -B quot_r.out.ml -

hcexpr.quot_r.TEST: quot_r.out.ml
	cpp -E -DQAST -D$(OVERSION_NAME) quot_r.ml | perl -p -e 's,<:,<:hc,' > hctmp.quot_r.ml
	$(LAUNCH) $(NOT_OCAMLFIND) preprocess -package camlp5_example.parser_quotations,camlp5.pr_r \
	-ppopt -l -ppopt 400 -ppopt -sep -ppopt '\n' \
	-syntax camlp5r hctmp.quot_r.ml | \
	perl -p -e 's,Camlp5_hashcons.HC.make_[a-z_]+,,g' | \
	perl -p -e 's,Camlp5_hashcons.HC,MLast,g' | \
	perl -p -e 's,Camlp5_migrate.FromHC,MLast,g' | \
	camlp5r pr_r.cmo -l 400 -sep '\n' -impl - | \
	$(DIFF) -I '(\*' -B quot_r.out.ml -


% : %.opt
	mkdir -p _build && ./$<

.SUFFIXES: .cmo .cmi .ml .mli .cmx .cma .cmxa .cmt .cmti

clean::
	rm -rf META *.cm* *.o *tmp* quot_r.out.ml
