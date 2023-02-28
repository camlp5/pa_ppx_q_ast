(**pp -syntax camlp5o -package sexp_example.parser *)
(* camlp5o *)
(* eg_sexp.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Fmt ;;

let nil = {| () |} |> Stream.of_string |> Grammar.Entry.parse Pa_sexp.sexp_eoi ;;
let l = {| (a b (c . ()) . d) |} |> Stream.of_string |> Grammar.Entry.parse Pa_sexp.sexp_eoi ;;

module NoVala = struct
let nil = {| () |} |> Stream.of_string |> Grammar.Entry.parse Pa_sexp.sexp_novala_eoi ;;
let l = {| (a b (c . ()) . d) |} |> Stream.of_string |> Grammar.Entry.parse Pa_sexp.sexp_novala_eoi ;;
end

module HC = struct
let nil = {| () |} |> Stream.of_string |> Grammar.Entry.parse Pa_sexp.sexp_hashcons_eoi ;;
let l = {| (a b (c . ()) . d) |} |> Stream.of_string |> Grammar.Entry.parse Pa_sexp.sexp_hashcons_eoi ;;
end

module Unique = struct
let nil = {| () |} |> Stream.of_string |> Grammar.Entry.parse Pa_sexp.sexp_unique_eoi ;;
let l = {| (a b (c . ()) . d) |} |> Stream.of_string |> Grammar.Entry.parse Pa_sexp.sexp_unique_eoi ;;
end
