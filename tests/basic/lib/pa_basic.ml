(** -syntax camlp5r -package camlp5.parser_quotations,camlp5.extend_m *)
(* camlp5r *)
(* pa_sexp.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Asttools;
open MLast;
open Pcaml;

value basic_eoi = Grammar.Entry.create gram "basic_eoi";

EXTEND
  GLOBAL: basic_eoi;

  basic: [
    [
      c = CHAR ; n = INT_l -> Basic.{ char = Plexing.eval_char c ; int32 = Int32.of_string n }
    ]
  ]
  ;

  basic_eoi: [ [ x = basic; EOI -> x ] ];

END;
