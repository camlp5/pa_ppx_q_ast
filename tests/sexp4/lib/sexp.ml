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

type position = {
  pos_fname : string vala;
  pos_lnum : int vala;
  pos_bol : int vala;
  pos_cnum : int vala;
}

let dummy_pos = {
  pos_fname = VaVal "";
  pos_lnum = VaVal 0;
  pos_bol = VaVal 0;
  pos_cnum = VaVal (-1);
}


let ocaml_position (fname, lnum, bolp, lnuml, bolpl, bp, ep) =
  let loc_at n lnum bolp =
    {pos_fname = if lnum = -1 then VaVal "" else VaVal fname;
     pos_lnum = VaVal lnum; pos_bol = VaVal bolp; pos_cnum = VaVal n}
  in
loc_at bp lnum bolp


let mkpos loc =
  let fname = Ploc.file_name loc in
  let bp = Ploc.first_pos loc in
  let ep = Ploc.last_pos loc in
  let lnum = Ploc.line_nb loc in
  let bolp = Ploc.bol_pos loc in
  let lnuml = Ploc.line_nb_last loc in
  let bolpl = Ploc.bol_pos_last loc in
  ocaml_position (fname, lnum, bolp, lnuml, bolpl, bp, ep)

type sexp =
    Atom of position vala * (string vala)
  | Cons of position vala * (sexp vala) * (sexp vala)
  | Nil of position vala

module NoVala = struct
type 'a novala = 'a

type position = {
  pos_fname : string novala;
  pos_lnum : int novala;
  pos_bol : int novala;
  pos_cnum : int novala;
}

let dummy_pos = {
  pos_fname = "";
  pos_lnum = 0;
  pos_bol = 0;
  pos_cnum = -1;
}

type sexp =
    Atom of position novala * (string novala)
  | Cons of position novala * (sexp novala) * (sexp novala)
  | Nil of position novala
end
