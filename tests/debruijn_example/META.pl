#!/usr/bin/env perl

use strict ;

our $destdir = shift @ARGV ;

print <<"EOF";
# Specifications for "debruijn_example"
description = "debruijn example parsing & quotation support"

package "runtime" (
  directory = "$destdir/debruijn_example"
  requires = "hashcons"
  archive(byte) = "debruijn.cmo debruijn_hashcons.cmo debruijn_migrate.cmo"
  archive(native) = "debruijn.cmx debruijn_hashcons.cmx debruijn_migrate.cmx"
)

package "parser" (
  requires(toploop) = "camlp5,debruijn_example.runtime"
  archive(toploop) = "pa_debruijn.cmo"

    requires(syntax,preprocessor) = "camlp5,fmt,debruijn_example.runtime,pa_ppx_q_ast.runtime"
    archive(syntax,preprocessor,-native) = "pa_debruijn.cmo"
    archive(syntax,preprocessor,native) = "pa_debruijn.cmx"

  package "link" (
  requires(byte) = "camlp5,fmt,debruijn_example.runtime"
  archive(byte) = "pa_debruijn.cmo"
  )
  requires = "camlp5,fmt,debruijn_example.runtime"
)

package "parser_quotations" (
  requires(toploop) = "camlp5,debruijn_example.parser"
  archive(toploop) = "q_ast_debruijn.cmo"

    requires(syntax,preprocessor) = "camlp5,fmt,debruijn_example.runtime,debruijn_example.parser,camlp5.parser_quotations"
    archive(syntax,preprocessor,-native) = "q_ast_debruijn.cmo"
    archive(syntax,preprocessor,native) = "q_ast_debruijn.cmx"

  package "link" (
  requires(byte) = "camlp5,fmt,debruijn_example.runtime,debruijn_example.parser,camlp5.parser_quotations"
  archive(byte) = "q_ast_debruijn.cmo"
  )
  requires = "camlp5,fmt,debruijn_example.runtime,debruijn_example.parser,camlp5.parser_quotations"
)

EOF
