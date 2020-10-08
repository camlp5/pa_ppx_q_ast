# Makefile,v
# Copyright (c) INRIA 2007-2017

WD=$(shell pwd)
TOP=../..

DESTDIR=
RM=rm

LAUNCH=env TOP=$(TOP) $(TOP)/tools/LAUNCH
OCAMLFIND=$(LAUNCH) ocamlfind
NOT_OCAMLFIND=$(LAUNCH) not-ocamlfind
MKCAMLP5=$(LAUNCH) mkcamlp5
PACKAGES := camlp5,pa_ppx_q_ast,pa_ppx.import,pa_ppx_hashcons,pa_ppx_migrate
SYNTAX = camlp5o
OCAMLCFLAGS += -ppopt -pa_passthru-debug 
OCAMLCFLAGS += -ppopt -pa_import-package -ppopt camlp5
OCAMLCFLAGS += -ppopt -pa_import-I -ppopt .


ML=\
	camlp5_ast.ml camlp5_hashcons.ml camlp5_migrate.ml pa_camlp5.ml q_ast_camlp5.ml \

MLI=
CMO := $(ML:.ml=.cmo)
CMI := $(ML:.ml=.cmi)
CMX := $(ML:.ml=.cmx)
CMT := $(ML:.ml=.cmt)
CMTI := $(MLI:.mli=.cmti)

all test: $(CMO) $(CMX) local-install

q_ast_camlp5.cmo: q_ast_camlp5.ml
	$(NOT_OCAMLFIND) preprocess $(OCAMLCFLAGS) -package $(PACKAGES),camlp5.pr_o -syntax camlp5o $< > $@.ppo.ml
	$(OCAMLFIND) ocamlc $(OCAMLCFLAGS) -package $(PACKAGES) -syntax camlp5o -c $@.ppo.ml
	$(OCAMLFIND) ocamlc $(OCAMLCFLAGS) -package $(PACKAGES) -syntax camlp5o -c $<

q_ast_camlp5.cmx: q_ast_camlp5.ml
	$(OCAMLFIND) ocamlopt $(OCAMLCFLAGS) -package $(PACKAGES) -syntax camlp5o -c $<

pa_camlp5.cmo: pa_camlp5.ml
	$(NOT_OCAMLFIND) preprocess -package camlp5.extend,camlp5.pr_r -syntax camlp5r $< > $@.ppo.ml
	$(OCAMLFIND) ocamlc -package camlp5.extend -syntax camlp5r -c $@.ppo.ml
	$(OCAMLFIND) ocamlc -package camlp5.extend -syntax camlp5r -c $<

pa_camlp5.cmx: pa_camlp5.ml
	$(OCAMLFIND) ocamlopt -package camlp5.extend -syntax camlp5r -c $<

HCTYPES=attribute_body case_branch \
	class_expr class_sig_item class_str_item class_type \
	ctyp expr extension_constructor generic_constructor \
	longid module_expr module_type patt payload poly_variant \
	sig_item str_item type_decl type_extension type_var with_constr

EXTTYPES= \
	attribute_body case_branch \
	class_expr class_sig_item class_str_item class_type \
	ctyp expr extension_constructor generic_constructor \
	longid module_expr module_type patt payload poly_variant \
	sig_item str_item type_decl type_extension type_var with_constr

SKIPTYPES=longid_lident attribute attributes_no_anti attributes
camlp5_migrate.ml: camlp5_migrate.ml.TMPL make-migrate Makefile
	./make-migrate inject $(HCTYPES) > inject_nodes.tmp
	./make-migrate inject_skip $(SKIPTYPES) > inject_skip_nodes.tmp
	./make-migrate project $(HCTYPES) > project_nodes.tmp
	./make-migrate project_skip $(SKIPTYPES) > project_skip_nodes.tmp
	./make-migrate binding $(EXTTYPES) > fun_bindings.tmp
	cppo -n camlp5_migrate.ml.TMPL > camlp5_migrate.ml

QTYPES= \
	attribute_body \
	class_expr class_sig_item class_str_item class_type \
	ctyp expr extension_constructor generic_constructor:constructor \
	longid:longident longid:extended_longident \
	module_expr module_type patt poly_variant \
	sig_item str_item type_decl type_extension with_constr

q_ast_camlp5.ml: q_ast_camlp5.ml.TMPL make-qast Makefile
	./make-qast ok $(QTYPES) > ok_quotations.tmp
	./make-qast hc $(QTYPES) > hc_quotations.tmp
	cppo -n q_ast_camlp5.ml.TMPL > q_ast_camlp5.ml

pa_camlp5.ml: pa_camlp5.ml.TMPL make-pa Makefile
	./make-pa pa $(QTYPES) > pa_grammars.tmp
	cppo -n pa_camlp5.ml.TMPL > pa_camlp5.ml

doc: $(CMT) $(CMTI)

KITS=$(CMO)

META: META.pl
	./META.pl > META

install::
	mkdir -p $(DESTDIR)/lib
	./META.pl $(DESTDIR)/lib > META
	$(NOT_OCAMLFIND) reinstall-if-diff camlp5_example -destdir $(DESTDIR)/lib META $(CMO) $(CMX) $(MLI:.mli=.cmi) $(KITS:.cmo=.o) $(CMI) $(wildcard *.cmt*)
	$(RM) -f META

local-install::
	$(MAKE) DESTDIR=$(WD)/$(TOP)/local-install/ install

.SUFFIXES: .cmo .cmi .ml .mli .cmx .cma .cmxa .cmt .cmti
.PRECIOUS: camlp5_migrate.ml.TMPL

clean::
	rm -rf META *.cm* *.o *.a *.tmp camlp5_migrate.ml q_ast_camlp5.ml pa_camlp5.ml

EXTERNAL := $(shell $(OCAMLFIND) query -predicates byte -format '%m' $(PACKAGES) | grep local-install)
$(CMO) $(CMI) $(CMX): $(EXTERNAL)

depend::
	echo "$(CMO) $(CMI) $(CMX): $(EXTERNAL)" > .depend.NEW
	$(OCAMLFIND) ocamldep -package $(PACKAGES) -syntax camlp5o *.ml *.mli >> .depend.NEW \
		&& mv .depend.NEW .depend

-include .depend

.ml.cmo:
	$(NOT_OCAMLFIND) preprocess $(OCAMLCFLAGS) -package $(PACKAGES),camlp5.pr_o -syntax $(SYNTAX) $< > $@.ppo.ml
	$(OCAMLFIND) ocamlc $(OCAMLCFLAGS) -package $(PACKAGES) -syntax $(SYNTAX) -c $@.ppo.ml
	$(OCAMLFIND) ocamlc $(OCAMLCFLAGS) -package $(PACKAGES) -syntax $(SYNTAX) -c $<

.ml.cmx:
	$(OCAMLFIND) ocamlopt $(OCAMLCFLAGS) -package $(PACKAGES) -syntax $(SYNTAX) -c $<
