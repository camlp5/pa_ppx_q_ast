(**pp -package pa_ppx_q_ast,pa_ppx.import -syntax camlp5o *)
(* camlp5r *)
(* pa_sexp.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Asttools
open MLast
open Pcaml

open Pa_lam
open Q_ast 

module Regular = struct
[%%import: Lam.lam]
[@@deriving q_ast {
       default_data_source_module = Lam
     ; entrypoints = [
         { name = "lam" ; grammar_entry = Pa_lam.lam_eoi ; type_name = lam }
       ]
}]
end

module OK = struct
[%%import: Lam_hashcons.OK.lam]
[@@deriving q_ast {
       default_data_source_module = Lam_hashcons.OK
     ; entrypoints = [
         { name = "oklam" ; grammar_entry = Pa_lam.lam_eoi ; type_name = lam }
       ]
}]
end

module Hashcons = struct

[%%import: Lam_hashcons.HC.lam]
[@@deriving q_ast {
    default_data_source_module = Lam_hashcons.HC
  ; default_quotation_source_module = Lam_migrate.FromHC
  ; hashconsed = true
  ; entrypoints = [
      { name = "hclam" ; grammar_entry = Pa_lam.lam_hashcons_eoi ; type_name = lam }
    ]
  ; node_mode = Hashcons
  }]
end

