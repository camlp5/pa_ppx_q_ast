(**pp -syntax camlp5o -package pa_ppx_q_ast,pa_ppx.import *)
(* camlp5r *)
(* pa_sexp.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Asttools
open MLast
open Pcaml

open Pa_ppx_base
open Q_ast_base

module MetaE = struct
  include Q_ast_base.E_MetaSig
  let int n = let loc = Ploc.dummy in <:expr< $int:string_of_int n$ >>
  open Pa_ppx_base
  open MLast
    let vala elem x =
      match Pa_ppx_q_ast_runtime.MetaE.vala elem x with
        <:expr< Ploc.VaVal $e$ >> -> e
      | e -> Ploc.raise (loc_of_expr e)
               (Failure Fmt.(str "Sexp_menhir_example.NoVala.vala: unexpected result expr:@ %a"
                               Pp_MLast.pp_expr e))
end

module MetaP = struct
  include Q_ast_base.P_MetaSig
  let int n = let loc = Ploc.dummy in <:patt< $int:string_of_int n$ >>
  open Pa_ppx_base
  open MLast
    let vala elem x =
      match Pa_ppx_q_ast_runtime.MetaP.vala elem x with
        <:patt< Ploc.VaVal $e$ >> -> e
      | e -> Ploc.raise (loc_of_patt e)
               (Failure Fmt.(str "Sexp_menhir_example.NoVala.vala: unexpected result patt:@ %a"
                               Pp_MLast.pp_patt e))
end


[%%import: Sexp.Pattern.sexp
  [@add type location = [%import: Location.t]
  ]
  [@with Location.t := location]
]
[@@deriving q_ast {
       default_data_source_module = Sexp.Normal
     ; default_quotation_source_module = Sexp.Pattern
     ; expr_meta_module = MetaE
     ; patt_meta_module = MetaP
     ; loc_mode = CustomLoc { loc_varname = __loc__ ; loc_type = [%typ: location] ; loc_function_name = location }
     ; pertype = {
         sexp = {
           custom_branches_code = function
           | Xtra (loc, s) → C.xtr (Sexp.ploc_of_location loc) s
         }
       }
     ; entrypoints = [
         { name = "sexp" ; from_string = Sexp_parse.pattern_sexp ; type_name = sexp }
       ]
}]

