(** -syntax camlp5o -package basic_example.parser *)
(* camlp5o *)
(* eg_sexp.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Fmt ;;

let t = {| 'c' |} |> Stream.of_string |> Grammar.Entry.parse Pa_basic.basic_eoi ;;
