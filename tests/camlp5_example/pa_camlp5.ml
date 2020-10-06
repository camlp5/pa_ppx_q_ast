(* camlp5r *)
(* pa_debruijn.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Asttools;
open MLast;
open Pcaml;

value expr_eoi = Q_ast.expr_eoi ;
value expr_hashcons_eoi = Grammar.Entry.create gram "expr_hashcons_eoi";

EXTEND
  GLOBAL: expr_eoi expr_hashcons_eoi;

  expr_hashcons_eoi: [ [ x = expr_eoi -> Camlp5_migrate.Inject.inject x ] ];

END;
