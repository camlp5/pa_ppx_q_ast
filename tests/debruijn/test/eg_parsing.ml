(** -syntax camlp5o -package camlp5.parser_quotations,hashcons,debruijn_example.parser *)
(* camlp5o *)
(* eg_parsing.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Fmt ;;

let v0 = Debruijn.Ref <:vala< 1 >> ;;
let v0' = {| 1 |} |> Stream.of_string |> Grammar.Entry.parse Pa_debruijn.term_eoi ;;
let skki = {| [][][](2 0 (1 0)) [][]1 [][]1 []0 |} |> Stream.of_string |> Grammar.Entry.parse Pa_debruijn.term_eoi ;;
