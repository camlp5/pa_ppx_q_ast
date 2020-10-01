(* camlp5o *)
(* eg_q_ast.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Fmt ;;

let loc = Ploc.dummy ;;

let rec pp0 pps = function
    <:lam< $var:x$ >> -> Fmt.(pf pps "%s" x)
  | x -> Fmt.(pf pps "(%a)" pp x)
and pp pps x = pp2 pps x
and pp2 pps = function
    <:lam< $lam:_M$ $lam:_N$ >> -> Fmt.(pf pps "%a %a" pp2 _M pp1 _N)
  | x -> pp1 pps x
and pp1 pps = function
    <:lam< [$var:x$]$lam:_M$ >> -> Fmt.(pf pps "[%s]%a" x pp1 _M)
  | x -> pp0 pps x
;;

let rec pp0 pps = function
    <:hclam< $var:x$ >> -> Fmt.(pf pps "%s" x)
  | x -> Fmt.(pf pps "(%a)" pp x)
and pp pps x = pp2 pps x
and pp2 pps = function
    <:hclam< $lam:_M$ $lam:_N$ >> -> Fmt.(pf pps "%a %a" pp2 _M pp1 _N)
  | x -> pp1 pps x
and pp1 pps = function
    <:hclam< [$var:x$]$lam:_M$ >> -> Fmt.(pf pps "[%s]%a" x pp1 _M)
  | x -> pp0 pps x
;;
