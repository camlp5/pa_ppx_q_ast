(**pp -syntax camlp5o -package lam_example.parser_quotations *)
(* camlp5o *)
(* eg_q_ast.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Fmt ;;
open Pa_ppx_utils ;;

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

let freevars =
  let fvrec = ref (fun _ -> assert false) in
  let rec fvrec0 = function
      <:hclam< $var:x$ >> -> [x]
    | <:hclam< $lam:_M$ $lam:_N$ >> -> (!fvrec _M)@(!fvrec _N)
    | <:hclam< [$var:x$]$lam:_M$ >> ->
      let fvM = !fvrec _M in
      Std.except x fvM in
  fvrec := Lam_hashcons.HC.memo_lam fvrec0 ;
  !fvrec
;;
   
let is_value = function
    <:hclam< $var:_$ >> -> true
  | <:hclam< [$var:_$]$lam:_$ >> -> true
  | _ -> false
;;

let captures x rho =
  List.exists (fun (_, _N) -> List.mem x (freevars _N)) rho
;;

let ctr = ref 0
let freshn ?num () =
  let num = match num with None -> 0 | Some s -> int_of_string s in
  if !ctr < num then ctr := num ;
  ctr := 1 + !ctr ;
  string_of_int !ctr

let fresh x =
  match Pcre.extract ~pat:"^(.+)([0-9]+)$" ~pos:0 x with
    [|_; id; num |] -> id^(freshn ~num:num ())
  | exception Not_found -> x^(freshn())
;;

let rec subst rho = function
    <:hclam< $var:x$ >> as z ->
    (match List.assoc x rho with
       y -> y
     | exception Not_found -> z)
  | <:hclam< $lam:_M$ $lam:_N$ >> ->
    <:hclam< $lam:subst rho _M$ $lam:subst rho _N$ >>
  | <:hclam< [$var:x$]$lam:_M$ >> ->
    if captures x rho then
      let x' = fresh x in
      let x'lam = <:hclam< $var:x'$ >> in
      subst rho <:hclam< [$var:x'$]$lam:subst [(x,x'lam)] _M$ >>
    else <:hclam< [$var:x$]$lam:subst rho _M$ >>
;;

let eta_simple_beta =
  Lam_hashcons.HC.memo_lam (function
        <:hclam< [$var:x$]($lam:_M$ $var:x'$) >> when x = x' && not (List.mem x (freevars _M)) -> _M
      | <:hclam< ([$var:x$]$lam:_M$ $lam:_N$) >> when is_value _N -> subst [(x, _N)] _M
      | z -> z)
;;

let beta = 
  Lam_hashcons.HC.memo_lam (function
        <:hclam< ([$var:x$]$lam:_M$ $lam:_N$) >> -> subst [(x, _N)] _M
      | z -> z)
;;

let rec fix f t =
  match f t with
    t' when t != t' -> fix f t'
  | t -> t
;;

let headnorm f =
  let hrec = ref (fun _ -> assert false) in
  let rec hrec0 = fun t ->
    match fix f t with
      <:hclam< ($lam:_M$ $lam:_N$) >> as z ->
      (match !hrec _M with
         _M' when _M != _M' -> !hrec <:hclam< ($lam:_M'$ $lam:_N$) >>
       | _ -> z)
    | z -> z in
  hrec := Lam_hashcons.HC.memo_lam hrec0 ;
  !hrec
;;

let hnf_beta = headnorm beta ;;

let norm f =
  let nrec = ref (fun _ -> assert false) in
  let nrec0 = fun t ->
    match headnorm f t with
      <:hclam< $var:_$ >> as z -> z
    | <:hclam< [$var:x$]$lam:_M$ >> -> <:hclam< [$var:x$]$lam:!nrec _M$ >>
    | <:hclam< ($lam:_M$ $lam:_N$) >> -> <:hclam< ($lam:!nrec _M$ $lam:!nrec _N$) >> in
  nrec := Lam_hashcons.HC.memo_lam nrec0 ;
  !nrec
;;

let nf_beta = norm beta ;;

let simplify = norm eta_simple_beta ;;

end
;;  
