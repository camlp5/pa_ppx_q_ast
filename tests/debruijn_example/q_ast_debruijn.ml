(* camlp5r *)
(* pa_sexp.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Asttools
open MLast
open Pcaml

open Pa_debruijn
open Q_ast 

module Regular = struct

[%%import: Debruijn.term]
[@@deriving q_ast {
    data_source_module = Debruijn
 }]

Quotation.add "debruijn"
  (apply_entry Pa_debruijn.term_eoi E.term P.term)
end

module OK = struct

[%%import: Debruijn_hashcons.OK.term]
[@@deriving q_ast {
    data_source_module = Debruijn_hashcons.OK
  }]

Quotation.add "okdebruijn"
  (apply_entry Pa_debruijn.term_eoi E.term P.term)
end

module Hashcons = struct

[%%import: Debruijn_hashcons.HC.term]
[@@deriving q_ast {
    data_source_module = Debruijn_hashcons.HC
  ; quotation_source_module = Debruijn_migrate.FromHC
  ; hashconsed = true
  }]

Quotation.add "hcdebruijn"
  (apply_entry Pa_debruijn.term_hashcons_eoi E.term P.term)
end
