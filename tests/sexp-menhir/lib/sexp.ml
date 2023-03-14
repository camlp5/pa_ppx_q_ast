(**pp -syntax camlp5o *)
(* camlp5o *)
(* sexp.ml,v *)

type sexp =
    Atom of Location.t * string
  | Cons of Location.t * sexp * sexp
  | Nil of Location.t
