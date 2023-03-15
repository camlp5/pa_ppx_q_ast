(**pp -syntax camlp5o -package pa_ppx_q_ast,pa_ppx.import $(IMPORT_OCAMLCFLAGS) *)
(* camlp5r *)
(* pa_sexp.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Asttools
open MLast
open Pcaml

open Pa_ppx_base
open Q_ast_base

module Regular = struct
[%%import: Sexp.Normal.sexp
  [@add type location = [%import: Location.t]
  ]
  [@with Location.t := location]
]
[@@deriving q_ast {
       default_data_source_module = Sexp.Normal
     ; loc_mode = CustomLoc { loc_varname = __loc__ ; loc_type = [%typ: location] ; loc_function_name = location }
     ; entrypoints = [
         { name = "sexp" ; from_string = Sexp_parse.sexp ; type_name = sexp }
       ]
}]

end
