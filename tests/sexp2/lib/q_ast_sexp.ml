(** -syntax camlp5o -package hashcons,pa_ppx_q_ast,pa_ppx.import,pa_ppx_unique.runtime $(IMPORT_OCAMLCFLAGS) *)
(* camlp5r *)
(* pa_sexp.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Asttools
open MLast
open Pcaml

open Pa_ppx_base
open Pa_sexp
open Q_ast 

module Regular = struct
type sexp = [%import: Sexp.sexp]
[@@deriving q_ast { data_source_module = Sexp }]

Quotation.add "sexp"
  (apply_entry Pa_sexp.sexp_eoi E.sexp P.sexp)
end
