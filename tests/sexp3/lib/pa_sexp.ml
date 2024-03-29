(**pp -syntax camlp5r -package camlp5.parser_quotations,camlp5.extend_m *)
(* camlp5r *)
(* pa_sexp.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Asttools;
open MLast;
open Pcaml;

value sexp_eoi = Grammar.Entry.create gram "sexp_eoi";

value sexp_atom loc a = Sexp.Atom loc a ;
value sexp_nil loc = Sexp.Nil loc ;
value sexp_cons loc e1 e2 = Sexp.Cons loc e1 e2 ;

EXTEND
  GLOBAL: sexp_eoi;

  sexp: [
    [
      a = V atom "atom" -> sexp_atom loc a
    | "(" ; l1 = LIST1 v_sexp ; opt_e2 = OPT [ "." ; e2 = v_sexp -> e2 ] ; ")" ->
      match opt_e2 with [
        None -> List.fold_right (fun vse1 se2 -> Sexp.Cons loc vse1 <:vala< se2 >>) l1 (sexp_nil loc)
      | Some ve2 ->
         let (last, l1) = sep_last l1 in
         List.fold_right (fun vse1 se2 -> Sexp.Cons loc vse1 <:vala< se2 >>) l1
           (Sexp.Cons loc last ve2)
      ]
    | "(" ; ")" ->
        sexp_nil loc
    ]
  ]
  ;

  v_sexp: [[ v = V sexp "exp" -> v ]];

  atom: [[ i = LIDENT -> i | i = UIDENT -> i | i = INT -> i ]] ;

  sexp_eoi: [ [ x = sexp; EOI -> x ] ];

END;
