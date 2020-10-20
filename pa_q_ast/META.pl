#!/usr/bin/env perl

use strict ;
BEGIN { push (@INC, "..") }
use Version ;

our $destdir = shift @ARGV ;

print <<"EOF";
# Specifications for the "pa_ppx_q_ast" preprocessor:
version = "$Version::version"
description = "pa_ppx_q_ast deriver"

  package "runtime" (
    requires = "camlp5.parser_quotations"
    archive(byte) = "pa_ppx_q_ast_runtime.cmo"
    archive(native) = "pa_ppx_q_ast_runtime.cmx"
  )

  requires(toploop) = "camlp5,pa_ppx.deriving_plugins.show,camlp5.parser_quotations,pa_ppx.params_runtime,pa_ppx_q_ast.runtime"
  archive(toploop) = "pa_deriving_q_ast.cmo"

    requires(syntax,preprocessor) = "camlp5,pa_ppx.deriving_plugins.show,camlp5.parser_quotations,pa_ppx.params_runtime,pa_ppx_q_ast.runtime"
    archive(syntax,preprocessor,-native) = "pa_deriving_q_ast.cmo"
    archive(syntax,preprocessor,native) = "pa_deriving_q_ast.cmx"

  package "link" (
  requires(byte) = "camlp5,pa_ppx.deriving_plugins.show.link,camlp5.parser_quotations.link,pa_ppx_q_ast.runtime"
  archive(byte) = "pa_deriving_q_ast.cmo"
  )
  requires = "camlp5,pa_ppx.deriving_plugins.show,pa_ppx.runtime,camlp5.parser_quotations,pa_ppx.params_runtime,pa_ppx_q_ast.runtime"

EOF
