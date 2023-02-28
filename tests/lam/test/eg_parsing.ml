(**pp -syntax camlp5o -package camlp5.parser_quotations,lam_example.parser *)
(* camlp5o *)
(* eg_parsing.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Fmt ;;

let v0 = Lam.Var <:vala< "x" >> ;;
let v0' = {| x |} |> Stream.of_string |> Grammar.Entry.parse Pa_lam.lam_eoi ;;
let skki = {| [x][y][z](x z (y z)) [x][y]x [x][y]x [x]x |} |> Stream.of_string |> Grammar.Entry.parse Pa_lam.lam_eoi ;;
