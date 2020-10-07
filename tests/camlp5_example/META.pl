#!/usr/bin/env perl

use strict ;

our $destdir = shift @ARGV ;

print <<"EOF";
# Specifications for "camlp5_example"
description = "camlp5 example parsing & quotation support"

package "runtime" (
  directory = "$destdir/camlp5_example"
  requires = "hashcons"
  archive(byte) = "camlp5_ast.cmo camlp5_hashcons.cmo camlp5_migrate.cmo"
  archive(native) = "camlp5_ast.cmx camlp5_hashcons.cmx camlp5_migrate.cmx"
)

package "parser" (
  requires(toploop) = "camlp5,camlp5_example.runtime,camlp5.parser_quotations"
  archive(toploop) = "pa_camlp5.cmo"

    requires(syntax,preprocessor) = "camlp5,fmt,camlp5_example.runtime,camlp5.parser_quotations"
    archive(syntax,preprocessor,-native) = "pa_camlp5.cmo"
    archive(syntax,preprocessor,native) = "pa_camlp5.cmx"

  package "link" (
  requires(byte) = "camlp5,fmt,camlp5_example.runtime,camlp5.parser_quotations.link"
  archive(byte) = "pa_camlp5.cmo"
  )
  requires = "camlp5,fmt,camlp5_example.runtime,camlp5.parser_quotations"
)

package "parser_quotations" (
  requires(toploop) = "camlp5,camlp5_example.parser"
  archive(toploop) = "q_ast_camlp5.cmo"

    requires(syntax,preprocessor) = "camlp5,fmt,camlp5_example.runtime,camlp5_example.parser,camlp5.parser_quotations"
    archive(syntax,preprocessor,-native) = "q_ast_camlp5.cmo"
    archive(syntax,preprocessor,native) = "q_ast_camlp5.cmx"

  package "link" (
  requires(byte) = "camlp5,fmt,camlp5_example.runtime,camlp5_example.parser,camlp5.parser_quotations"
  archive(byte) = "q_ast_camlp5.cmo"
  )
  requires = "camlp5,fmt,camlp5_example.runtime,camlp5_example.parser,camlp5.parser_quotations"
)

EOF
