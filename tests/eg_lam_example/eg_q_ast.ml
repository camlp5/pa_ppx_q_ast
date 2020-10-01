(* camlp5o *)
(* eg_q_ast.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Fmt ;;

let loc = Ploc.dummy ;;

module Regular = struct

let v0 = <:lam< x >>
let v1 = <:lam< [x][y]x >>

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


let rec copy = function
    <:lam:< $var:x$ >> -> <:lam:< $var:x$ >>
  | <:lam:< $lam:_M$ $lam:_N$ >> -> <:lam:< $lam:copy _M$ $lam:copy _N$ >>
  | <:lam:< [$var:x$]$lam:_M$ >> -> <:lam:< [$var:x$]$lam:copy _M$ >>

end ;;

module OK = struct

let v0 = <:oklam< x >>
let v1 = <:oklam< [x][y]x >>

let rec pp0 pps = function
    <:oklam< $var:x$ >> -> Fmt.(pf pps "%s" x)
  | x -> Fmt.(pf pps "(%a)" pp x)
and pp pps x = pp2 pps x
and pp2 pps = function
    <:oklam< $lam:_M$ $lam:_N$ >> -> Fmt.(pf pps "%a %a" pp2 _M pp1 _N)
  | x -> pp1 pps x
and pp1 pps = function
    <:oklam< [$var:x$]$lam:_M$ >> -> Fmt.(pf pps "[%s]%a" x pp1 _M)
  | x -> pp0 pps x


let rec copy = function
    <:oklam:< $var:x$ >> -> <:oklam:< $var:x$ >>
  | <:oklam:< $lam:_M$ $lam:_N$ >> -> <:oklam:< $lam:copy _M$ $lam:copy _N$ >>
  | <:oklam:< [$var:x$]$lam:_M$ >> -> <:oklam:< [$var:x$]$lam:copy _M$ >>
end
;;  

module HC = struct

let v0 = <:hclam< x >>
let v1 = <:hclam< [x][y]x >>

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


let rec copy = function
    <:hclam:< $var:x$ >> -> <:hclam:< $var:x$ >>
  | <:hclam:< $lam:_M$ $lam:_N$ >> -> <:hclam:< $lam:copy _M$ $lam:copy _N$ >>
  | <:hclam:< [$var:x$]$lam:_M$ >> -> <:hclam:< [$var:x$]$lam:copy _M$ >>
end
;;  
