(* camlp5o *)
(* sexp.ml,v *)

open Ploc

type term =
    Ref of int vala
  | Abs of term vala
  | App of term vala * term vala
