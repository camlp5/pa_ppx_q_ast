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
[@@deriving q_ast { data_source_module = Lam }]

Quotation.add "lam"
  (apply_entry Pa_lam.lam_eoi E.lam P.lam)
end

module Hashcons = struct

module MetaE = struct
  include Q_ast_base.E_MetaSig
end

module MetaP = struct
  include Q_ast_base.P_MetaSig
end

[%%import: Lam_hashcons.HC.lam]
[@@deriving q_ast {
    data_source_module = Lam_migrate.Project
  ; quotation_source_module = Lam_migrate.Project
  ; expr_meta_module = MetaE
  ; patt_meta_module = MetaP
  ; hashconsed = true
  }]

Quotation.add "hclam"
  (apply_entry Pa_lam.lam_hashcons_eoi E.lam P.lam)
end

