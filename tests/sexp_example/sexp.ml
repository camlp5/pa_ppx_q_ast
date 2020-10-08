(* camlp5o *)
(* sexp.ml,v *)

open Ploc

module NoVala = struct
type 'a novala = 'a
type sexp =
    Atom of (string novala)
  | Cons of (sexp novala) * (sexp novala)
  | Nil
end

type sexp =
    Atom of (string vala)
  | Cons of (sexp vala) * (sexp vala)
  | Nil
