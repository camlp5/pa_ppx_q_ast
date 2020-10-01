(* camlp5r *)
(* pa_sexp.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Asttools
open MLast
open Pcaml


module Ploc= struct
include Ploc

let pp ppf x = Fmt.(const string "<loc>" ppf ())
end

[%%import: MLast.expr
    [@add [%%import: MLast.loc]]
    [@add [%%import: MLast.type_var]]
    [@add [%%import: 'a Ploc.vala]]
    [@with Ploc.vala := vala]
]
[@@deriving q_ast { data_source_module = MLast }]
