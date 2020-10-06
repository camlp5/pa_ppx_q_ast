(* camlp5o *)
(* eg_q_ast.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Fmt ;;

let loc = Ploc.dummy ;;

module Regular = struct

let v0 = <:expr< 0 >>
let v1 = <:expr< 1 + 2 * 3 / 4 >>

let rec pp0 pps = function
    <:expr< $int:n$ >> -> Fmt.(pf pps "%s" n)
  | x -> Fmt.(pf pps "(%a)" pp x)
and pp pps x = pp3 pps x
and pp3 pps = function
    <:expr< $e1$ + $e2$ >> -> Fmt.(pf pps "%a + %a" pp3 e1 pp2 e2)
  | <:expr< $e1$ - $e2$ >> -> Fmt.(pf pps "%a - %a" pp3 e1 pp2 e2)
  | x -> pp2 pps x
and pp2 pps = function
    <:expr< $e1$ * $e2$ >> -> Fmt.(pf pps "%a * %a" pp2 e1 pp1 e2)
  | <:expr< $e1$ / $e2$ >> -> Fmt.(pf pps "%a / %a" pp2 e1 pp1 e2)
  | x -> pp1 pps x
and pp1 pps x = pp0 pps x

let rec copy = function
  <:expr:< $int:n$ >> -> <:expr:< $int:n$ >>
  | <:expr:< $e1$ + $e2$ >> -> <:expr:< $e1$ + $e2$ >>
  | <:expr:< $e1$ - $e2$ >> -> <:expr:< $e1$ - $e2$ >>
  | <:expr:< $e1$ * $e2$ >> -> <:expr:< $e1$ * $e2$ >>
  | <:expr:< $e1$ / $e2$ >> -> <:expr:< $e1$ / $e2$ >>

end ;;


module OK = struct

let v0 = <:okexpr< 0 >>
let v1 = <:okexpr< 1 + 2 * 3 / 4 >>

let rec pp0 pps = function
    <:okexpr< $int:n$ >> -> Fmt.(pf pps "%s" n)
  | x -> Fmt.(pf pps "(%a)" pp x)
and pp pps x = pp3 pps x
and pp3 pps = function
    <:okexpr< $e1$ + $e2$ >> -> Fmt.(pf pps "%a + %a" pp3 e1 pp2 e2)
  | <:okexpr< $e1$ - $e2$ >> -> Fmt.(pf pps "%a - %a" pp3 e1 pp2 e2)
  | x -> pp2 pps x
and pp2 pps = function
    <:okexpr< $e1$ * $e2$ >> -> Fmt.(pf pps "%a * %a" pp2 e1 pp1 e2)
  | <:okexpr< $e1$ / $e2$ >> -> Fmt.(pf pps "%a / %a" pp2 e1 pp1 e2)
  | x -> pp1 pps x
and pp1 pps x = pp0 pps x

let rec copy = function
  <:okexpr:< $int:n$ >> -> <:okexpr:< $int:n$ >>
  | <:okexpr:< $e1$ + $e2$ >> -> <:okexpr:< $e1$ + $e2$ >>
  | <:okexpr:< $e1$ - $e2$ >> -> <:okexpr:< $e1$ - $e2$ >>
  | <:okexpr:< $e1$ * $e2$ >> -> <:okexpr:< $e1$ * $e2$ >>
  | <:okexpr:< $e1$ / $e2$ >> -> <:okexpr:< $e1$ / $e2$ >>

end ;;

module HC = struct

let v0 = <:hcexpr< 0 >>
let v1 = <:hcexpr< 1 + 2 * 3 / 4 >>

let rec pp0 pps = function
    <:hcexpr< $int:n$ >> -> Fmt.(pf pps "%s" n)
  | x -> Fmt.(pf pps "(%a)" pp x)
and pp pps x = pp3 pps x
and pp3 pps = function
    <:hcexpr< $e1$ + $e2$ >> -> Fmt.(pf pps "%a + %a" pp3 e1 pp2 e2)
  | <:hcexpr< $e1$ - $e2$ >> -> Fmt.(pf pps "%a - %a" pp3 e1 pp2 e2)
  | x -> pp2 pps x
and pp2 pps = function
    <:hcexpr< $e1$ * $e2$ >> -> Fmt.(pf pps "%a * %a" pp2 e1 pp1 e2)
  | <:hcexpr< $e1$ / $e2$ >> -> Fmt.(pf pps "%a / %a" pp2 e1 pp1 e2)
  | x -> pp1 pps x
and pp1 pps x = pp0 pps x

let rec copy = function
  <:hcexpr:< $int:n$ >> -> <:hcexpr:< $int:n$ >>
  | <:hcexpr:< $e1$ + $e2$ >> -> <:hcexpr:< $e1$ + $e2$ >>
  | <:hcexpr:< $e1$ - $e2$ >> -> <:hcexpr:< $e1$ - $e2$ >>
  | <:hcexpr:< $e1$ * $e2$ >> -> <:hcexpr:< $e1$ * $e2$ >>
  | <:hcexpr:< $e1$ / $e2$ >> -> <:hcexpr:< $e1$ / $e2$ >>

end ;;
