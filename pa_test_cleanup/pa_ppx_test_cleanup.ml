(**pp -syntax camlp5r -package camlp5.parser_quotations,camlp5.extfun *)
(* camlp5r *)
(* pa_here.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Pa_ppx_base ;
open Pa_passthru ;
open Ppxutil ;
open Pa_ppx_deriving ;

value rec rewrite_str_item0 arg = fun [
  <:str_item:< [@@@"end"] >> -> []
| <:str_item:< declare $list:l$ end >> ->
  List.concat_map (rewrite_str_item0 arg) l
| <:str_item:< type $list:_$ >> -> []
| z -> [z]
]
and rewrite_structure arg sil =
  List.concat_map (rewrite_str_item0 arg) sil

and rewrite_implem arg (si_loc_l, st) =
  let sil = List.map fst si_loc_l in
  let sil = rewrite_structure arg sil in
  (List.map (fun si -> (si, MLast.loc_of_str_item si)) sil, st)
;
value install () = 
let ef = EF.mk () in 
let ef = EF.{ (ef) with
            structure = extfun ef.structure with [
                z ->
    fun arg fallback ->
      Some (rewrite_structure arg z)
  ] } in
let ef = EF.{ (ef) with
            implem = extfun ef.implem with [
                z ->
    fun arg fallback ->
      Some (rewrite_implem arg z)
  ] } in
  Pa_passthru.(install { name = "pa_test_cleanup"; ef =  ef ; pass = None ; before = [] ; after = ["pa_deriving";"pa_quotation_test"] })
;

install();
