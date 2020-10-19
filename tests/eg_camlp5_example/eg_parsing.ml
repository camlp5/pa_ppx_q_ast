(* camlp5o *)
(* eg_parsing.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Fmt ;;

module OK = struct
let x = {| x |} |> Stream.of_string |> Grammar.Entry.parse Pcaml.expr_eoi
let id = {| function x -> x |} |> Stream.of_string |> Grammar.Entry.parse Pcaml.expr_eoi
end

module HC = struct
let x = {| x |} |> Stream.of_string |> Grammar.Entry.parse Pa_camlp5.expr_hashcons_eoi
let id = {| function x -> x |} |> Stream.of_string |> Grammar.Entry.parse Pa_camlp5.expr_hashcons_eoi
end
