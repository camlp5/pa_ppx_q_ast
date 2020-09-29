(* camlp5r *)
(* pa_sexp.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Asttools
open MLast
open Pcaml

open Pa_lam
open Q_ast 

type lam = [%import: Lam.lam]
[@@deriving q_ast { source_module = Lam }]

Quotation.add "lam"
  (apply_entry Pa_lam.lam_eoi E.lam P.lam)

