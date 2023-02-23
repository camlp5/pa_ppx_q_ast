# Makefile,v
# Copyright (c) INRIA 2007-2017

WD=$(shell pwd)
TOP=../../..
include $(TOP)/config/Makefile.sub

DESTDIR=

PACKAGES := fmt,camlp5,pa_ppx_q_ast,pa_ppx_hashcons,pa_ppx_migrate
TARGET_PACKAGE=camlp5_example

export IMPORT_OCAMLCFLAGS += -ppopt -pa_passthru-debug 
export IMPORT_OCAMLCFLAGS += -ppopt -pa_import-package -ppopt camlp5
export IMPORT_OCAMLCFLAGS += -ppopt -pa_import-I -ppopt .

ML=\
	camlp5_ast.ml camlp5_hashcons.ml camlp5_migrate.ml pa_camlp5.ml q_ast_camlp5.ml \

MLI=
ALLSRC=$(ML) $(MLI)
ALLPACKAGES=$(PACKAGES)

test: all

all: $(CMO) $(CMX)
	$(MAKE) local-install

HCTYPES=attribute_body \
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

SKIPTYPES=longid_lident attribute attributes_no_anti attributes case_branch
camlp5_migrate.ml: camlp5_migrate.ml.TMPL make-migrate Makefile
	./make-migrate inject $(HCTYPES) > inject_nodes.tmp
	./make-migrate inject_skip $(SKIPTYPES) > inject_skip_nodes.tmp
	./make-migrate project $(HCTYPES) > project_nodes.tmp
	./make-migrate project_skip $(SKIPTYPES) > project_skip_nodes.tmp
	./make-migrate to_binding $(EXTTYPES) > to_fun_bindings.tmp
	./make-migrate from_binding $(EXTTYPES) > from_fun_bindings.tmp
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

TARGET_INSTALL_FILES=$(FILES_TARGET_INSTALL_FILES)
install:: default_install

local-install::
	$(MAKE) DESTDIR=$(WD)/$(TOP)/local-install/ install

.SUFFIXES: .cmo .cmi .ml .mli .cmx .cma .cmxa .cmt .cmti
.PRECIOUS: camlp5_migrate.ml.TMPL

clean::
	rm -rf META *.cm* *.o *.a *.tmp camlp5_migrate.ml q_ast_camlp5.ml pa_camlp5.ml

camlp5_hashcons.cmo: camlp5_ast.cmo camlp5_ast.cmi
camlp5_hashcons.cmx: camlp5_ast.cmx camlp5_ast.cmi
camlp5_migrate.cmo: camlp5_hashcons.cmo camlp5_hashcons.cmi
camlp5_migrate.cmx: camlp5_hashcons.cmx camlp5_hashcons.cmi
q_ast_camlp5.cmo: camlp5_hashcons.cmo camlp5_hashcons.cmi
q_ast_camlp5.cmx: camlp5_hashcons.cmx camlp5_hashcons.cmi
