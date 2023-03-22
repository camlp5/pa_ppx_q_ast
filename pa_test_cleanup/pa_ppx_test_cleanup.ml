(**pp -syntax camlp5r -package camlp5.parser_quotations,camlp5.extfun *)
(* camlp5r *)
(* pa_here.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Pa_ppx_base ;
open Pa_passthru ;
open Ppxutil ;
open Pa_ppx_deriving ;

value rec rewrite_str_item arg = fun [
  <:str_item:< [@@@"end"] >> ->
  <:str_item< declare end >>
| <:str_item:< declare $list:l$ end >> ->
  let l = List.map (rewrite_str_item arg) l in
  <:str_item< declare $list:l$ end >>
| <:str_item:< type $list:_$ >> ->
  <:str_item< declare end >>
| z -> z
]
;

value install () = 
let ef = EF.mk () in 
let ef = EF.{ (ef) with
            str_item = extfun ef.str_item with [
                z ->
    fun arg fallback ->
      Some (rewrite_str_item arg z)
  ] } in
  Pa_passthru.(install { name = "pa_test_cleanup"; ef =  ef ; pass = None ; before = [] ; after = ["pa_deriving"] })
;

install();
