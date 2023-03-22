(**pp -syntax camlp5o -package sexp_menhir_example.parser_quotations *)
(* camlp5o *)
(* eg_sexp.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Fmt

let rec atoms = function
    <:sexp:< () >> -> []
  | <:sexp:< $atom:a$ >> -> [a]
  | <:sexp:< ( () . $cdr$ ) >> -> atoms cdr
  | <:sexp:< ( $atom:a$ . $cdr$ ) >> -> a::(atoms cdr)
  | <:sexp:< ( ( $caar$ . $cdar$ ) . $cdr$ ) >> ->
    atoms <:sexp< ( $caar$ . ( $cdar$ . $cdr$ ) ) >>

open Sexp.Normal
let rec atoms' =
  function
    Nil _ -> []
  | Atom (_, a) -> [a]
  | Cons(loc, Cons(_, caar, cdar), cdr) ->
      atoms' (Cons(loc, caar, Cons (loc, cdar, cdr)))
  | Cons(_, Nil _, cdr) -> atoms' cdr
  | Cons(_, Atom(_, a), cdr) -> a :: atoms' cdr
