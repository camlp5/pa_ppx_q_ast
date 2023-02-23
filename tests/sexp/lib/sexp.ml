(** -syntax camlp5o *)
(* camlp5o *)
(* sexp.ml,v *)

module Ploc = struct
  include Ploc
let preeq_t x y = x = y
let prehash_t x = Hashtbl.hash x
let hash_t = prehash_t
end

open Ploc

module NoVala = struct
type 'a novala = 'a
type sexp =
    Atom of Ploc.t * (string novala)
  | Cons of Ploc.t * (sexp novala) * (sexp novala)
  | Nil of Ploc.t
end

type sexp =
    Atom of Ploc.t * (string vala)
  | Cons of Ploc.t * (sexp vala) * (sexp vala)
  | Nil of Ploc.t
