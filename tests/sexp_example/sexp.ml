(* camlp5o *)
(* sexp.ml,v *)

open Ploc

type sexp =
    Atom of (string vala)
  | Cons of (sexp vala) * (sexp vala)
  | Nil
