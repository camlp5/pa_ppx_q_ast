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
let preeq_location x y = x = y
let prehash_location x = Hashtbl.hash x
let hash_location = prehash_location

module NoVala = struct
type 'a novala = 'a
type sexp =
    Atom of location * (string novala)
  | Cons of location * (sexp novala) * (sexp novala)
  | Nil of location
end

type sexp =
    Atom of location * (string vala)
  | Cons of location * (sexp vala) * (sexp vala)
  | Nil of location
