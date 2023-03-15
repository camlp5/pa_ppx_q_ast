(**pp -syntax camlp5o *)
(* camlp5o *)
(* sexp.ml,v *)

module Normal = struct
type sexp =
    Atom of Location.t * string
  | Cons of Location.t * sexp * sexp
  | Nil of Location.t
end
