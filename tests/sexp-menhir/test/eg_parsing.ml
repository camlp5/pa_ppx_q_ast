(**pp -syntax camlp5o -package sexp_menhir_example.parser *)
(* camlp5o *)
(* eg_sexp.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Fmt ;;

module Pattern = struct

let nil = {| () |} |> Sexp_parse.pattern_sexp
let l = {| (a b (c . ()) . d) |} |> Sexp_parse.pattern_sexp
end

let nil = {| () |} |> Sexp_parse.sexp
let l = {| (a b (c . ()) . d) |} |> Sexp_parse.sexp

