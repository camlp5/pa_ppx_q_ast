(** -package camlp5.parser_quotations,camlp5.extend_m -syntax camlp5r *)
(* camlp5r *)
(* pa_debruijn.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Asttools;
open MLast;
open Pcaml;
open Debruijn;

value term_eoi = Grammar.Entry.create gram "term_eoi";
value term_hashcons_eoi = Grammar.Entry.create gram "term_hashcons_eoi";

EXTEND
  GLOBAL: term_eoi term_hashcons_eoi;

  term: [
    "apply" LEFTA
    [ l = LIST1 (V (term LEVEL "abs") "term") ->
      Pcaml.unvala (List.fold_left (fun lhs rhs -> <:vala< App lhs rhs >>) (List.hd l) (List.tl l)) ]
  | "abs"
    [ "["; "]" ; e = V (term LEVEL "abs") "term" -> Abs e ]
  |  "var" [ n = V INT "ref" -> Ref (vala_map int_of_string n)
    | "(" ; e = SELF ; ")" -> e
    ]
  ]
  ;

  term_eoi: [ [ x = term; EOI -> x ] ];
  term_hashcons_eoi: [ [ x = term; EOI -> Debruijn_migrate.ToHC.term x ] ];

END;
