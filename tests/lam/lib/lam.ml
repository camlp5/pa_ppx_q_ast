(* camlp5o *)
(* sexp.ml,v *)

open Ploc

type lam =
    Var of string vala
  | Lam of string vala * lam vala
  | App of lam vala * lam vala
