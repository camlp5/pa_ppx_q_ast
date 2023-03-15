(**pp -syntax camlp5o -package pa_ppx.deriving_plugins.std *)
(* camlp5o *)
(* sexp.ml,v *)

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
  | Atom (_, Ploc.VaAnt s) -> Fmt.(pf pps "$atom:%s$" s)
  | Cons(_, l, (Cons _ as r)) ->
     Fmt.(pf pps "%a %a" pp_hum l pp_aux r)
  | Cons(_, l, (Nil _)) ->
     Fmt.(pf pps "%a" pp_hum l)
  | Cons(_, l, (Atom _ as r)) ->
     Fmt.(pf pps "%a . %a" pp_hum l pp_aux r)
  | Nil _ -> Fmt.(pf pps "()")
  | Xtra (_, s) -> Fmt.(pf pps "$%s$" s)

and pp_hum pps = function
    Cons _ as l -> Fmt.(pf pps "(%a)" pp_aux l)
  | l -> pp_aux pps l

and show_hum e = Fmt.(str "%a" pp_hum e)

end
