#!/usr/bin/env perl

use strict ;

our $destdir = shift @ARGV ;

print <<"EOF";
# Specifications for "sexp_example"
description = "sexp example parsing & quotation support"

package "runtime" (
  directory = "$destdir/sexp_example"
  archive(toploop) = "sexp.cmo sexp_hashcons.cmo sexp_unique.cmo sexp_migrate.cmo"
  archive(syntax,preprocessor,-native) = "sexp.cmo sexp_hashcons.cmo sexp_unique.cmo sexp_migrate.cmo"
  requires = "hashcons,pa_ppx_unique.runtime"
)

package "parser" (
  requires(toploop) = "camlp5,sexp_example.runtime"
  archive(toploop) = "pa_sexp.cmo"

    requires(syntax,preprocessor) = "camlp5,fmt,sexp_example.runtime"
    archive(syntax,preprocessor,-native) = "pa_sexp.cmo"
    archive(syntax,preprocessor,native) = "pa_sexp.cmx"

  package "link" (
  requires(byte) = "camlp5,fmt,sexp_example.runtime"
  archive(byte) = "pa_sexp.cmo"
  )
  requires = "camlp5,fmt,sexp_example.runtime"
)

package "parser_quotations" (
  requires(toploop) = "camlp5,sexp_example.parser,pa_ppx.base,pa_ppx_q_ast.runtime"
  archive(toploop) = "q_ast_sexp.cmo"

    requires(syntax,preprocessor) = "camlp5,fmt,sexp_example.runtime,sexp_example.parser,camlp5.parser_quotations,pa_ppx_q_ast.runtime,pa_ppx.base"
    archive(syntax,preprocessor,-native) = "q_ast_sexp.cmo"
    archive(syntax,preprocessor,native) = "q_ast_sexp.cmx"

  package "link" (
  requires(byte) = "camlp5,fmt,sexp_example.runtime,sexp_example.parser,camlp5.parser_quotations,pa_ppx.base.link"
  archive(byte) = "q_ast_sexp.cmo"
  )
  requires = "camlp5,fmt,sexp_example.runtime,sexp_example.parser,camlp5.parser_quotations,pa_ppx.base"
)

EOF
