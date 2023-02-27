(** -syntax camlp5o -package basic_example.parser_quotations *)
(* camlp5o *)
(* eg_sexp.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Fmt ;;

let rec pp pps = function
    <:basic< 'c' >> -> ()
;;
