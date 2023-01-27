(** -syntax camlp5o -package debruijn_example.parser_quotations *)
(* camlp5o *)
(* eg_q_ast.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Fmt ;;

let loc = Ploc.dummy ;;

module Regular = struct

let v0 = <:debruijn< 0 >>
let v1 = <:debruijn< [][]1 >>

let rec pp0 pps = function
    <:debruijn< $ref:n$ >> -> Fmt.(pf pps "%d" n)
  | x -> Fmt.(pf pps "(%a)" pp x)
and pp pps x = pp2 pps x
and pp2 pps = function
    <:debruijn< $term:_M$ $term:_N$ >> -> Fmt.(pf pps "%a %a" pp2 _M pp1 _N)
  | x -> pp1 pps x
and pp1 pps = function
    <:debruijn< []$term:_M$ >> -> Fmt.(pf pps "[]%a" pp1 _M)
  | x -> pp0 pps x

let rec copy = function
    <:debruijn:< $ref:n$ >> -> <:debruijn:< $ref:n$ >>
  | <:debruijn:< $term:_M$ $term:_N$ >> -> <:debruijn:< $term:copy _M$ $term:copy _N$ >>
  | <:debruijn:< []$term:_M$ >> -> <:debruijn:< []$term:copy _M$ >>

end ;;

module OK = struct

let v0 = <:okdebruijn< 0 >>
let v1 = <:okdebruijn< [][]1 >>

let rec pp0 pps = function
    <:okdebruijn< $ref:x$ >> -> Fmt.(pf pps "%d" x)
  | x -> Fmt.(pf pps "(%a)" pp x)
and pp pps x = pp2 pps x
and pp2 pps = function
    <:okdebruijn< $term:_M$ $term:_N$ >> -> Fmt.(pf pps "%a %a" pp2 _M pp1 _N)
  | x -> pp1 pps x
and pp1 pps = function
    <:okdebruijn< []$term:_M$ >> -> Fmt.(pf pps "[]%a" pp1 _M)
  | x -> pp0 pps x


let rec copy = function
    <:okdebruijn:< $ref:x$ >> -> <:okdebruijn:< $ref:x$ >>
  | <:okdebruijn:< $term:_M$ $term:_N$ >> -> <:okdebruijn:< $term:copy _M$ $term:copy _N$ >>
  | <:okdebruijn:< []$term:_M$ >> -> <:okdebruijn:< []$term:copy _M$ >>
end
;;  

module HC = struct

let v0 = <:hcdebruijn< 0 >>
let v1 = <:hcdebruijn< [][]1 >>

let rec pp0 pps = function
    <:hcdebruijn< $ref:x$ >> -> Fmt.(pf pps "%d" x)
  | x -> Fmt.(pf pps "(%a)" pp x)
and pp pps x = pp2 pps x
and pp2 pps = function
    <:hcdebruijn< $term:_M$ $term:_N$ >> -> Fmt.(pf pps "%a %a" pp2 _M pp1 _N)
  | x -> pp1 pps x
and pp1 pps = function
    <:hcdebruijn< []$term:_M$ >> -> Fmt.(pf pps "[]%a" pp1 _M)
  | x -> pp0 pps x


let rec copy = function
    <:hcdebruijn:< $ref:x$ >> -> <:hcdebruijn:< $ref:x$ >>
  | <:hcdebruijn:< $term:_M$ $term:_N$ >> -> <:hcdebruijn:< $term:copy _M$ $term:copy _N$ >>
  | <:hcdebruijn:< []$term:_M$ >> -> <:hcdebruijn:< []$term:copy _M$ >>
end
;;  
