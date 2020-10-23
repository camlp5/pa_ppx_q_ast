(* camlp5o *)
(* eg_sexp.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Fmt ;;

module Regular = struct
let rec pp pps = function
    <:sexp:< () >> -> Fmt.(pf pps "()")
  | <:sexp:< ( $exp:a$ . $exp:b$ ) >> -> Fmt.(pf pps "(%a . %a)" pp a pp b)
  | <:sexp:< $atom:a$ >> -> Fmt.(pf pps "%a" string a)
;;

let subst rho e =
  let rec srec = function
    <:sexp:< () >> -> <:sexp< () >>
  | <:sexp:< ( $exp:a$ . $exp:b$ ) >> -> 
    <:sexp:< ( $exp:srec a$ . $exp:srec b$ ) >>
  | <:sexp:< $atom:a$ >> as z ->
    if List.mem_assoc a rho then List.assoc a rho
    else z
in srec e
;;

let rec atoms = function
    <:sexp:< () >> -> []
  | <:sexp:< $atom:a$ >> -> [a]
  | <:sexp:< ( () . $exp:cdr$ ) >> -> atoms cdr
  | <:sexp:< ( $atom:a$ . $exp:cdr$ ) >> -> a::(atoms cdr)
  | <:sexp:< ( ( $exp:caar$ . $exp:cdar$ ) . $exp:cdr$ ) >> ->
    atoms <:sexp< ( $exp:caar$ . ( $exp:cdar$ . $exp:cdr$ ) ) >>
end
;;

module NoVala = struct

  let loc_of_sexp = function
    <:sexpnovala:< () >> -> loc
  | <:sexpnovala:< $atom:_$ >> -> loc
  | <:sexpnovala:< ( $exp:_$ . $exp:_$ ) >> -> loc

open Sexp.NoVala
let rec atoms = function
    <:sexpnovala:< () >> -> []
  | <:sexpnovala:< $atom:a$ >> -> [a]
  | <:sexpnovala:< ( () . $exp:cdr$ ) >> -> atoms cdr
  | <:sexpnovala:< ( $atom:a$ . $exp:cdr$ ) >> -> a::(atoms cdr)
  | <:sexpnovala:< ( ( $exp:caar$ . $exp:cdar$ ) . $exp:cdr$ ) >> ->
    atoms <:sexpnovala< ( $exp:caar$ . ( $exp:cdar$ . $exp:cdr$ ) ) >>

let rec atoms' =
  function
    Nil _ -> []
  | Atom (_, a) -> [a]
  | Cons(loc, Cons(_, caar, cdar), cdr) ->
      atoms (Cons(loc, caar, Cons (loc, cdar, cdr)))
  | Cons(_, Nil _, cdr) -> atoms cdr
  | Cons(_, Atom(_, a), cdr) -> a :: atoms cdr
end

module HC = struct

  let loc_of_sexp = function
    <:hcsexp:< () >> -> loc
  | <:hcsexp:< $atom:_$ >> -> loc
  | <:hcsexp:< ( $exp:_$ . $exp:_$ ) >> -> loc

let rec atoms = function
    <:hcsexp:< () >> -> []
  | <:hcsexp:< $atom:a$ >> -> [a]
  | <:hcsexp:< ( () . $exp:cdr$ ) >> -> atoms cdr
  | <:hcsexp:< ( $atom:a$ . $exp:cdr$ ) >> -> a::(atoms cdr)
  | <:hcsexp:< ( ( $exp:caar$ . $exp:cdar$ ) . $exp:cdr$ ) >> ->
    atoms <:hcsexp< ( $exp:caar$ . ( $exp:cdar$ . $exp:cdr$ ) ) >>

let car = function
  <:hcsexp< ( $exp:a$ . $exp:_$ ) >> -> a
let cdr = function
  <:hcsexp< ( $exp:_$ . $exp:a$ ) >> -> a
end

module Unique = struct

  let loc_of_sexp = function
    <:unsexp:< () >> -> loc
  | <:unsexp:< $atom:_$ >> -> loc
  | <:unsexp:< ( $exp:_$ . $exp:_$ ) >> -> loc

let rec atoms = function
    <:unsexp:< () >> -> []
  | <:unsexp:< $atom:a$ >> -> [a]
  | <:unsexp:< ( () . $exp:cdr$ ) >> -> atoms cdr
  | <:unsexp:< ( $atom:a$ . $exp:cdr$ ) >> -> a::(atoms cdr)
  | <:unsexp:< ( ( $exp:caar$ . $exp:cdar$ ) . $exp:cdr$ ) >> ->
    atoms <:unsexp< ( $exp:caar$ . ( $exp:cdar$ . $exp:cdr$ ) ) >>


let car = function
  <:unsexp< ( $exp:a$ . $exp:_$ ) >> -> a
let cdr = function
  <:unsexp< ( $exp:_$ . $exp:a$ ) >> -> a
end
