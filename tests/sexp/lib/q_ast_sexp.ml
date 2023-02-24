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
[@@deriving q_ast {
       data_source_module = Sexp
     ; entrypoints = [
         { name = "sexp" ; grammar_entry = Pa_sexp.sexp_eoi ; type_name = sexp }
       ]
}]

end

module NoVala = struct
  module MetaE = struct
    include Pa_ppx_q_ast_runtime.MetaE
    let vala elem x =
      match Pa_ppx_q_ast_runtime.MetaE.vala elem x with
        <:expr< Ploc.VaVal $e$ >> -> e
      | e -> Ploc.raise (loc_of_expr e)
               (Failure Fmt.(str "Sexp_example.NoVala.vala: unexpected result expr:@ %a"
                               Pp_MLast.pp_expr e))
  end
  module MetaP = struct
    include Pa_ppx_q_ast_runtime.MetaP
    let vala elem x =
      match Pa_ppx_q_ast_runtime.MetaP.vala elem x with
        <:patt< Ploc.VaVal $e$ >> -> e
      | e -> Ploc.raise (loc_of_patt e)
               (Failure Fmt.(str "Sexp_example.NoVala.vala: unexpected result patt:@ %a"
                               Pp_MLast.pp_patt e))
  end

type sexp = [%import: Sexp.sexp]
[@@deriving q_ast {
    data_source_module = Sexp.NoVala
  ; quotation_source_module = Sexp
  ; expr_meta_module = MetaE
  ; patt_meta_module = MetaP
  ; entrypoints = [ { name = "sexpnovala"; grammar_entry = Pa_sexp.sexp_eoi; type_name = sexp } ]
  ; node_mode = Normal
  ; loc_mode = AutoLoc
  }]

end

module Hashcons = struct

[%%import: Sexp_hashcons.HC.sexp]
[@@deriving q_ast {
    data_source_module = Sexp_hashcons.HC
  ; quotation_source_module = Sexp_migrate.FromHC
  ; hashconsed = true
  ; entrypoints = [ { name = "hcsexp"; grammar_entry = Pa_sexp.sexp_hashcons_eoi; type_name = sexp } ]
  ; node_mode = Hashcons
  ; loc_mode = AutoLoc
  }]
end

module Unique = struct

[%%import: Sexp_unique.UN.sexp]
[@@deriving q_ast {
    data_source_module = Sexp_unique.UN
  ; quotation_source_module = Sexp_migrate.FromUnique
  ; uniqified = true
  ; entrypoints = [ { name = "unsexp"; grammar_entry = Pa_sexp.sexp_unique_eoi; type_name = sexp } ]
  ; node_mode = Unique
  ; loc_mode = AutoLoc
  }]
end
