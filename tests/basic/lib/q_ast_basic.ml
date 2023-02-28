(**pp -syntax camlp5o -package hashcons,pa_ppx_q_ast,pa_ppx.import,pa_ppx_unique.runtime $(IMPORT_OCAMLCFLAGS) *)
(* camlp5r *)
(* pa_sexp.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Asttools
open MLast
open Pcaml

open Pa_ppx_base
open Pa_basic

module Regular = struct
[%%import: Basic.t]
[@@deriving q_ast {
       default_data_source_module = Basic
     ; loc_mode = NoLoc
     ; entrypoints = [
         {name = "basic"; grammar_entry = Pa_basic.basic_eoi ; type_name = t }
       ]
}]

end
