(**pp -syntax camlp5o *)
(* camlp5o *)
(* sexp.ml,v *)

module Ploc = struct
  include Ploc
let preeq_t x y = x = y
let prehash_t x = Hashtbl.hash x
let hash_t = prehash_t
end

open Ploc


type sexp =
    Atom of location * location_stack * (string vala)
  | Cons of location * location_stack * (sexp vala) * (sexp vala)
  | Nil of location * location_stack
and location = Ploc.t
and location_stack = Ploc.t list

let loc_of_sexp = function
    Atom (loc, _, _) -> loc
  | Cons (loc, _, _, _) -> loc
  | Nil (loc, _) -> loc

let location_stack_of_sexp = function
    Atom (_, l, _) -> l
  | Cons (_, l, _, _) -> l
  | Nil (_, l) -> l
