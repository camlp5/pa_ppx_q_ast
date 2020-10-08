(* camlp5o *)
(* eg_sexp.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Fmt ;;

let loc = Ploc.dummy ;;

let rec pp pps = function
    <:sexp< () >> -> Fmt.(pf pps "()")
  | <:sexp< ( $exp:a$ . $exp:b$ ) >> -> Fmt.(pf pps "(%a . %a)" pp a pp b)
  | <:sexp< $atom:a$ >> -> Fmt.(pf pps "%a" string a)
;;

let subst rho e =
  let rec srec = function
    <:sexp< () >> -> <:sexp< () >>
  | <:sexp< ( $exp:a$ . $exp:b$ ) >> -> 
    <:sexp< ( $exp:srec a$ . $exp:srec b$ ) >>
  | <:sexp< $atom:a$ >> as z ->
    if List.mem_assoc a rho then List.assoc a rho
    else z
in srec e
;;

module NoVala = struct
open Sexp.NoVala
let rec atoms = function
    <:sexpnovala< () >> -> []
  | <:sexpnovala< $atom:a$ >> -> [a]

  | <:sexpnovala< ( () . $exp:cdr$ ) >> -> atoms cdr
  | <:sexpnovala< ( $atom:a$ . $exp:cdr$ ) >> -> a::(atoms cdr)
  | <:sexpnovala< ( ( $exp:caar$ . $exp:cdar$ ) . $exp:cdr$ ) >> ->
    atoms <:sexpnovala< ( $exp:caar$ . ( $exp:cdar$ . $exp:cdr$ ) ) >>

let rec atoms =
  function
    Nil -> []
  | Atom a -> [a]
  | Cons(Cons(caar, cdar), cdr) ->
      atoms (Cons(caar, Cons (cdar, cdr)))
  | Cons(Nil, cdr) -> atoms cdr
  | Cons(Atom a, cdr) -> a :: atoms cdr
end

let rec atoms = function
    <:sexp< () >> -> []
  | <:sexp< $atom:a$ >> -> [a]

  | <:sexp< ( () . $exp:cdr$ ) >> -> atoms cdr
  | <:sexp< ( $atom:a$ . $exp:cdr$ ) >> -> a::(atoms cdr)
  | <:sexp< ( ( $exp:caar$ . $exp:cdar$ ) . $exp:cdr$ ) >> ->
    atoms <:sexp< ( $exp:caar$ . ( $exp:cdar$ . $exp:cdr$ ) ) >>


