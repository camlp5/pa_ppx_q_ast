(**pp -syntax camlp5r -package camlp5.parser_quotations *)
(* camlp5r *)
(* pa_here.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Pa_ppx_base ;
open Pa_passthru ;
open Ppxutil ;

value rewrite_str_item arg = fun [
  <:str_item:< [%%here] >> ->
    let pos = start_position_of_loc loc in
    let e = quote_position loc pos in
    <:str_item< $exp:e$ >>
| _ -> assert False
]
;

value install () = 
let ef = EF.mk () in 
let ef = EF.{ (ef) with
            str_item = extfun ef.str_item with [
    <:str_item:< [%%here] >> as z ->
    fun arg fallback ->
      Some (rewrite_str_item arg z)
  ] } in
  Pa_passthru.(install { name = "pa_mkast"; ef =  ef ; pass = None ; before = [] ; after = [] })
;

install();
