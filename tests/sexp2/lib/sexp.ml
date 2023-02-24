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

type location = Ploc.t

type sexp =
    Atom of location * (string vala)
  | Cons of location * (sexp vala) * (sexp vala)
  | Nil of location
