(** -package camlp5.parser_quotations,camlp5.extend_m -syntax camlp5r *)
(* camlp5r *)
(* pa_lam.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Asttools;
open MLast;
open Pcaml;
open Lam;

value lam_eoi = Grammar.Entry.create gram "lam_eoi";
value lam_hashcons_eoi = Grammar.Entry.create gram "lam_hashcons_eoi";

EXTEND
  GLOBAL: lam_eoi lam_hashcons_eoi;

  lam: [
    "apply" LEFTA
    [ l = LIST1 (V (lam LEVEL "abs") "lam") ->
      Pcaml.unvala (List.fold_left (fun lhs rhs -> <:vala< App lhs rhs >>) (List.hd l) (List.tl l)) ]
  | "abs"
    [ "[" ; id = V LIDENT "var" ; "]" ; e = V (lam LEVEL "abs") "lam" -> Lam id e ]
  |  "var" [ id = V LIDENT "var" -> Var id
    | "(" ; e = SELF ; ")" -> e
    ]
  ]
  ;

  lam_eoi: [ [ x = lam; EOI -> x ] ];
  lam_hashcons_eoi: [ [ x = lam; EOI -> Lam_migrate.ToHC.lam x ] ];

END;
