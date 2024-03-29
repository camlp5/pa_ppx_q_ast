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
type location = [%import: Sexp.location]
type sexp = [%import: Sexp.sexp]
[@@deriving q_ast {
       default_data_source_module = Sexp
     ; loc_mode = CustomLoc { loc_varname = __loc__ ; loc_type = [%typ: location] ; loc_function_name = location }
     ; entrypoints = [
         {name = "sexp"; grammar_entry = Pa_sexp.sexp_eoi ; type_name = sexp }
       ]
}]

end
