#!/usr/bin/env perl

use strict ;

our $destdir = shift @ARGV ;

print <<"EOF";
# Specifications for "lam_example"
description = "lam example parsing & quotation support"

package "runtime" (
  directory = "$destdir/lam_example"
  archive(toploop) = "lam.cmo"
)

package "parser" (
  requires(toploop) = "camlp5"
  archive(toploop) = "pa_lam.cmo"

    requires(syntax,preprocessor) = "camlp5,fmt,lam_example.runtime"
    archive(syntax,preprocessor,-native) = "pa_lam.cmo"
    archive(syntax,preprocessor,native) = "pa_lam.cmx"

  package "link" (
  requires(byte) = "camlp5,fmt,lam_example.runtime"
  archive(byte) = "pa_lam.cmo"
  )
  requires = "camlp5,fmt,lam_example.runtime"
)

package "parser_quotations" (
  requires(toploop) = "camlp5,lam_example.parser"
  archive(toploop) = "q_ast_lam.cmo"

    requires(syntax,preprocessor) = "camlp5,fmt,lam_example.runtime,lam_example.parser,camlp5.parser_quotations"
    archive(syntax,preprocessor,-native) = "q_ast_lam.cmo"
    archive(syntax,preprocessor,native) = "q_ast_lam.cmx"

  package "link" (
  requires(byte) = "camlp5,fmt,lam_example.runtime,lam_example.parser,camlp5.parser_quotations"
  archive(byte) = "q_ast_lam.cmo"
  )
  requires = "camlp5,fmt,lam_example.runtime,lam_example.parser,camlp5.parser_quotations"
)

EOF
