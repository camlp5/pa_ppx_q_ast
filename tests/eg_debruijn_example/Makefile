# Makefile,v
# Copyright (c) INRIA 2007-2017

include Makefile.include

PACKAGES := $(PACKAGES),fmt

ML=\
	eg_parsing.ml eg_q_ast.ml \

CMO := $(ML:.ml=.cmo)
CMI := $(ML:.ml=.cmi)
CMX := $(ML:.ml=.cmx)
CMT := $(ML:.ml=.cmt)
CMTI := $(MLI:.mli=.cmti)

TESTS= lamh.byte lamh.opt \

all: $(CMO) $(TESTS)

test: $(TESTS:.byte=)

% : %.opt
	mkdir -p _build && ./$<

lamh.byte: lamh.cmo
	$(OCAMLFIND) ocamlc $(DEBUG) $(WARNERR) -package $(PACKAGES),debruijn_example.runtime \
	-linkpkg -linkall $^ -o $@

lamh.opt: lamh.cmx
	$(OCAMLFIND) ocamlopt $(DEBUG) $(WARNERR) -package $(PACKAGES),debruijn_example.runtime \
	-linkpkg -linkall $^ -o $@

eg_parsing.cmo: eg_parsing.ml
	$(MAKE) -f Makefile.one PACKAGES="$(PACKAGES),camlp5.quotations,hashcons,debruijn_example.parser" $@

lamh.cmo: lamh.ml
	$(MAKE) -f Makefile.one PACKAGES="$(PACKAGES),hashcons,debruijn_example.parser_quotations" $@

lamh.cmx: lamh.ml
	$(MAKE) -f Makefile.one PACKAGES="$(PACKAGES),hashcons,debruijn_example.parser_quotations" $@

eg_q_ast.cmo: eg_q_ast.ml
	$(MAKE) -f Makefile.one PACKAGES="$(PACKAGES),debruijn_example.parser_quotations" $@

.SUFFIXES: .cmo .cmi .ml .mli .cmx .cma .cmxa .cmt .cmti

clean::
	rm -rf META *.cm* *.o

EXTERNAL := $(shell $(OCAMLFIND) query -predicates byte -format '%m' $(PACKAGES) | grep local-install)
$(CMO) $(CMI) $(CMX): $(EXTERNAL)

depend::
	echo "$(CMO) $(CMI) $(CMX): $(EXTERNAL)" > .depend.NEW
	$(OCAMLFIND) ocamldep $(DEBUG) -package $(PACKAGES),camlp5,camlp5.quotations,debruijn_example.parser_quotations -syntax camlp5o $(ML) >> .depend.NEW \
		&& mv .depend.NEW .depend

-include .depend
