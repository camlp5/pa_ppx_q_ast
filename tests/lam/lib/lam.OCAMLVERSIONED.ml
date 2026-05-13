(* camlp5o *)
(* sexp.ml,v *)

open Ploc

type lam =
    Var of string vala
  | Lam of string vala * lam vala
#if OCAML_VERSION >= (5,4,0)
  | App of (f: lam vala * lam vala)
#else
  | App of (lam vala * lam vala)
#endif
