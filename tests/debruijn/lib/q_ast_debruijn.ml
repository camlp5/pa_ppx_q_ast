(**pp -package pa_ppx_q_ast,pa_ppx.import -syntax camlp5o *)
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
    default_data_source_module = Debruijn
     ; entrypoints = [
         { name = "debruijn" ; grammar_entry = Pa_debruijn.term_eoi ; type_name = term }
       ]
 }]
end

module OK = struct

[%%import: Debruijn_hashcons.OK.term]
[@@deriving q_ast {
    default_data_source_module = Debruijn_hashcons.OK
     ; entrypoints = [
         { name = "okdebruijn" ; grammar_entry = Pa_debruijn.term_eoi ; type_name = term }
       ]
  }]
end

module Hashcons = struct

[%%import: Debruijn_hashcons.HC.term]
[@@deriving q_ast {
    default_data_source_module = Debruijn_hashcons.HC
  ; default_quotation_source_module = Debruijn_migrate.FromHC
  ; hashconsed = true
  ; entrypoints = [
      { name = "hcdebruijn" ; grammar_entry = Pa_debruijn.term_hashcons_eoi ; type_name = term }
    ]
  ; node_mode = Hashcons
  }]
end
