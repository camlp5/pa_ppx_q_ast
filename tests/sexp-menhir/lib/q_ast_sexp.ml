(**pp -syntax camlp5o -package pa_ppx_q_ast,pa_ppx.import $(IMPORT_OCAMLCFLAGS) *)
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
    let xtr s = <:expr< $lid:s$ >>
    let vala elem x =
      match x with
        Ploc.VaVal p -> elem p
      | Ploc.VaAnt s -> xtr s
end

module MetaP = struct
  include Q_ast_base.P_MetaSig
  let int n = let loc = Ploc.dummy in <:patt< $int:string_of_int n$ >>
  open Pa_ppx_base
  open MLast
    let xtr = function
        "_" -> <:patt< _ >>
       | s -> <:patt< $lid:s$ >>
    let vala elem x =
      match x with
        Ploc.VaVal p -> elem p
      | Ploc.VaAnt "_" -> <:patt< _ >>
      | Ploc.VaAnt s -> xtr s
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
           | Xtra (loc, s) → C.xtr s
         }
    }
     ; entrypoints = [
         { name = "sexp" ; from_string = Sexp_parse.pattern_sexp ; type_name = sexp }
       ]
}]

