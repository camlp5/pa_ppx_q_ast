#!/usr/bin/env perl

use strict ;
BEGIN { push (@INC, "..") }
use Version ;

our $destdir = shift @ARGV ;

print <<"EOF";
# Specifications for the "pa_ppx_q_ast" preprocessor:
version = "$Version::version"
description = "pa_ppx_q_ast deriver"

  requires(toploop) = "camlp5,pa_ppx.deriving_plugins.show"
  archive(toploop) = "pa_deriving_q_ast.cmo"

    requires(syntax,preprocessor) = "camlp5,pa_ppx.deriving_plugins.show"
    archive(syntax,preprocessor,-native) = "pa_deriving_q_ast.cmo"
    archive(syntax,preprocessor,native) = "pa_deriving_q_ast.cmx"

  package "link" (
  requires(byte) = "camlp5,pa_ppx.deriving_plugins.show.link"
  archive(byte) = "pa_deriving_q_ast.cmo"
  )
  requires = "camlp5,pa_ppx.deriving_plugins.show,pa_ppx.runtime"

EOF
