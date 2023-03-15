(**pp -syntax camlp5o -package pa_ppx.deriving_plugins.std,pa_ppx_regexp *)
(* camlp5o *)
(* sexp.ml,v *)

open Pa_ppx_base
open Ppxutil

let ploc_of_location loc =
  let open Location in
  let open Lexing in
  Ploc.make_loc loc.loc_start.pos_fname loc.loc_start.pos_lnum loc.loc_start.pos_bol (loc.loc_start.pos_cnum, loc.loc_end.pos_cnum) ""

let make_antiquotation typestr loc payload =
  let open Location in
  let sp = loc.loc_start.pos_cnum in
  let ep = loc.loc_end.pos_cnum in
  Printf.sprintf "%d,%d:%s:%s" sp ep typestr payload

let unmk_antiquotation s =
  match [%match {|^([0-9]+),([0-9]+):([^:]*):(.*)$|} / strings (!1,!2,!3,!4)] s with
    Some (sps, eps, typestr, payload) -> (sps, eps, typestr, payload)
  | None -> Fmt.(failwithf "cannot destructure antiquotation string <<%s>>" s)

let pp_antiquotation pps antis =
  let (_, _, ty, pay) = unmk_antiquotation antis in
  if ty = "" then
    Fmt.(pf pps "$%s$" pay)
  else
    Fmt.(pf pps "$%s:%s$" ty pay)

module Normal = struct
type loc_t = Location.t
let equal_loc_t a b = true
let pp_loc_t pps _ = Fmt.(pf pps "<loc>")

type sexp =
    Atom of loc_t * string
  | Cons of loc_t * sexp * sexp
  | Nil of loc_t [@@deriving show,eq]

let rec pp_aux pps = function
    Atom (_, s) -> Fmt.(pf pps "%s" s)
  | Cons(_, l, (Cons _ as r)) ->
     Fmt.(pf pps "%a %a" pp_hum l pp_aux r)
  | Cons(_, l, (Nil _)) ->
     Fmt.(pf pps "%a" pp_hum l)
  | Cons(_, l, (Atom _ as r)) ->
     Fmt.(pf pps "%a . %a" pp_hum l pp_aux r)
  | Nil _ -> Fmt.(pf pps "()")

and pp_hum pps = function
    Cons _ as l -> Fmt.(pf pps "(%a)" pp_aux l)
  | l -> pp_aux pps l

and show_hum e = Fmt.(str "%a" pp_hum e)
end

module Pattern = struct
type sexp =
    Atom of Location.t * string Ploc.vala
  | Cons of Location.t * sexp * sexp
  | Nil of Location.t
  | Xtra of Location.t * string

let rec pp_aux pps = function
    Atom (_, Ploc.VaVal s) -> Fmt.(pf pps "%s" s)
  | Atom (_, Ploc.VaAnt s) -> pp_antiquotation pps s
  | Cons(_, l, (Cons _ as r)) ->
     Fmt.(pf pps "%a %a" pp_hum l pp_aux r)
  | Cons(_, l, (Nil _)) ->
     Fmt.(pf pps "%a" pp_hum l)
  | Cons(_, l, (Atom _ as r)) ->
     Fmt.(pf pps "%a . %a" pp_hum l pp_aux r)
  | Nil _ -> Fmt.(pf pps "()")
  | Xtra (_, s) -> pp_antiquotation pps s

and pp_hum pps = function
    Cons _ as l -> Fmt.(pf pps "(%a)" pp_aux l)
  | l -> pp_aux pps l

and show_hum e = Fmt.(str "%a" pp_hum e)

end
