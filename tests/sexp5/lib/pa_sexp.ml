(**pp -syntax camlp5r -package camlp5.parser_quotations,camlp5.extend_m *)
(* camlp5r *)
(* pa_sexp.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Asttools;
open MLast;
open Pcaml;

value sexp_eoi = Grammar.Entry.create gram "sexp_eoi";
value sexp_novala_eoi = Grammar.Entry.create gram "sexp_novala_eoi";

value sexp_atom loc a = Sexp.(Atom (vala_map Position.mkpos loc) a) ;
value sexp_nil loc = Sexp.(Nil (vala_map Position.mkpos loc)) ;
value sexp_cons loc e1 e2 = Sexp.(Cons (vala_map Position.mkpos loc) e1 e2) ;

EXTEND
  GLOBAL: sexp_eoi sexp_novala_eoi;

  location : [ [ -> loc ] ] ;

  sexp: [
    [
      l = V location "loc" ; a = V atom "atom" -> sexp_atom l a
    | l = V location "loc" ; "(" ; l1 = LIST1 v_sexp ; opt_e2 = OPT [ "." ; e2 = v_sexp -> e2 ] ; ")" ->
      match opt_e2 with [
        None -> List.fold_right (fun vse1 se2 -> sexp_cons l vse1 <:vala< se2 >>) l1 (sexp_nil l)
      | Some ve2 ->
         let (last, l1) = sep_last l1 in
         List.fold_right (fun vse1 se2 -> sexp_cons l vse1 <:vala< se2 >>) l1
           (sexp_cons l last ve2)
      ]
    | l = V location "loc" ; "(" ; ")" ->
        sexp_nil l
    ]
  ]
  ;

  v_sexp: [[ v = V sexp "exp" -> v ]];

  atom: [[ i = LIDENT -> i | i = UIDENT -> i | i = INT -> i ]] ;

  sexp_eoi: [ [ x = sexp; EOI -> x ] ];
  sexp_novala_eoi: [ [ x = sexp; EOI -> Sexp_migrate.ToNoVala.sexp x ] ];

END;
