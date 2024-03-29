(**pp -syntax camlp5o -package hashcons,pa_ppx_q_ast,pa_ppx.import,pa_ppx_unique.runtime *)
(* camlp5r *)
(* pa_sexp.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Asttools
open MLast
open Pcaml

open Pa_ppx_base
open Pa_sexp

module Regular = struct
[%%import: Sexp.sexp]
[@@deriving q_ast {
       default_data_source_module = Sexp
     ; custom_type = [
         ([%typ: location], {
           pattern = (fun ctxt _ -> <:patt< _ >>)
         ; expression = (fun ctxt _ -> <:expr< loc >>)
         ; function_name = location
         })
       ; ([%typ: location_stack], {
           pattern = (fun ctxt _ -> <:patt< _ >>)
         ; expression = (fun ctxt _ -> <:expr< [] >>)
         ; function_name = location_stack
         })
       ]
     ; loc_mode = NoLoc
     ; entrypoints = [
         {name = "sexp"; grammar_entry = Pa_sexp.sexp_eoi ; type_name = sexp }
       ]
}]

end
