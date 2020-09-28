(* camlp5r *)
(* pa_sexp.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Asttools
open MLast
open Pcaml

open Pa_sexp
open Q_ast 

type sexp = [%import: Sexp.sexp]
[@@deriving q_ast { source_module = Sexp }]

Quotation.add "sexp"
  (apply_entry Pa_sexp.sexp_eoi E.sexp P.sexp)

