(** -syntax camlp5o -package camlp5_example.parser_quotations *)
(* camlp5o *)
(* eg_q_ast.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Fmt ;;
open Pa_ppx_utils ;;
open Pa_ppx_base ;;
open Ppxutil ;;

module Regular = struct

let v0 = let loc = Ploc.dummy in <:expr< 0 >>
let v1 = let loc = Ploc.dummy in <:expr< 1 + 2 * 3 / 4 >>

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

let v0 = let loc = Ploc.dummy in <:okexpr< 0 >>
let v1 = let loc = Ploc.dummy in <:okexpr< 1 + 2 * 3 / 4 >>

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

let loc_of_type_decl = function
    <:oktype_decl:< $lid:_$ = $_$ >> -> loc

end ;;

module HC = struct

let v0 = let loc = Ploc.dummy in <:hcexpr< 0 >>
let v1 = let loc = Ploc.dummy in <:hcexpr< 1 + 2 * 3 / 4 >>

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
 
exception Fail

module FVS0 = struct
  type t = string list

  let ofList l = Std2.hash_uniq l
  let union l1 l2 = Std.union l1 l2
  let free_in x l = List.mem x l
  let not_free_in x l = not (List.mem x l)
  let except id l = Std.except id l
  let exceptl ~bound l = Std.subtract l bound
end

module OrNot = struct
  type 'a t = KNOWN of 'a | UNKNOWN
  let known x = KNOWN x
  let unknown () = UNKNOWN
  let bind x f =
    match x with
      UNKNOWN -> UNKNOWN
    | KNOWN l -> f l
  let is_known = function
    KNOWN _ -> true | UNKNOWN -> false
  let is_unknown = function
    KNOWN _ -> false | UNKNOWN -> true

  let get_known = function
    KNOWN l -> l | UNKNOWN -> raise Fail
end

module FVS = struct
  type t = FVS0.t OrNot.t
  let ofList l = OrNot.known (FVS0.ofList l)
  let union l1 l2 =
    OrNot.bind l1 (fun l1 -> OrNot.bind l2 (fun l2 -> OrNot.known (FVS0.union l1 l2)))
  let free_in x l =
    if OrNot.is_known l then
      FVS0.free_in x (OrNot.get_known l)
    else false
  let not_free_in x l =
    if OrNot.is_known l then
      FVS0.not_free_in x (OrNot.get_known l)
    else false
  let except id l =
    OrNot.bind l (fun l -> OrNot.known (FVS0.except id l))
  let exceptl ~bound l =
    OrNot.bind l (fun l -> OrNot.known (FVS0.exceptl ~bound l))
end

let patt_bound_vars p =
  let rec patrec = function
      <:hcpatt< $lid:x$ >> -> [x]
    | <:hcpatt< $p1$ $p2$ >> -> (patrec p1)@(patrec p2)
    | <:hcpatt< ( $list:l$ ) >> -> List.concat (List.map patrec l)
    | <:hcpatt< $longid:_$ >> -> []
  in
  let bvs = (patrec p) in
  if not (Std.distinct bvs) then raise Fail ;
  bvs

let freevars =
  let memo_freevars = ref (fun _ -> assert false) in
  let fvrec e = !memo_freevars e in
  let fvrec0 = function
      <:hcexpr< $lid:id$ >> -> FVS.ofList [id]
    | <:hcexpr< function $list:branches$ >> ->
      let fv_branch (p, whene, rhs) =
        let bvars = patt_bound_vars p in
        let whene_fvs = match uv whene with None -> FVS.ofList [] | Some e -> fvrec e in
        let rhs_fvs = fvrec rhs in
        FVS.exceptl ~bound:bvars (FVS.union whene_fvs rhs_fvs) in
      List.fold_left FVS.union (FVS.ofList[]) (List.map fv_branch branches)
    | <:hcexpr< $e1$ $e2$ >> -> FVS.union (fvrec e1) (fvrec e2)
    | <:hcexpr< ( $list:l$ ) >> -> List.fold_left FVS.union (FVS.ofList[]) (List.map fvrec l)
  in
  memo_freevars := Camlp5_hashcons.HC.memo_expr fvrec0 ;
  fvrec

module Rho = struct
  type t = (string * Camlp5_hashcons.HC.expr) list

  let captures bv rho =
    List.exists (fun (_, rhs) -> FVS.free_in bv (freevars rhs)) rho
  let capturing bvs rho =
    bvs |> Std.filter (fun bv -> captures bv rho)
end

module Fresh = struct
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
end

let patt_alpha_subst rho p =
  let rec patrec = function
      <:hcpatt:< $lid:x$ >> ->
      <:hcpatt< $lid:if List.mem_assoc x rho then List.assoc x rho else x$ >>

    | <:hcpatt:< $p1$ $p2$ >> -> <:hcpatt< $patrec p1$ $patrec p2$ >>
    | <:hcpatt:< ( $list:l$ ) >> -> <:hcpatt< ( $list:List.map patrec l$ ) >>
    | <:hcpatt:< $longid:_$ >> as z -> z
  in
  patrec p


let rec subst rho z =
  let fvz = freevars z in
  if OrNot.is_unknown fvz then raise Fail ;
  match z with
    <:hcexpr:< $lid:id$ >> as z ->
    if List.mem_assoc id rho then List.assoc id rho else z
  | <:hcexpr:< $e1$ $e2$ >> -> <:hcexpr< $subst rho e1$ $subst rho e2$ >>
  | <:hcexpr:< ( $list:l$ ) >> -> <:hcexpr< ( $list:List.map (subst rho) l$ ) >>
  | <:hcexpr:< function $list:branches$ >> as z ->
    (* keep only entries [x -> N] where x is free in z *)
    let rho = rho |> Std.filter (fun (id, _) -> FVS.free_in id fvz) in
    let subst_branch (p, whene, rhs) =
      let bvs = patt_bound_vars p in
      (* remove entries [x -> N] where x is bound in p *)
      let rho = rho |> Std.filter (fun (id, _) -> not (List.mem id bvs)) in
      let (p, whene, rhs) = match Rho.capturing bvs rho with
          [] -> (p, whene, rhs)
        | l -> 
          let patt_alpha_rho = List.map (fun id -> (id, Fresh.fresh id)) l in
          let p = patt_alpha_subst patt_alpha_rho  p in
          let alpha_rho = List.map (fun (id, id') -> (id, <:hcexpr< $lid:id'$ >>)) patt_alpha_rho in
          (p, Pcaml.vala_map (Option.map (subst alpha_rho)) whene, subst alpha_rho rhs) in

      assert (Rho.capturing (patt_bound_vars p) rho = []) ;
      (p, Pcaml.vala_map (Option.map (subst rho)) whene, subst rho rhs) in
    <:hcexpr< function $list:List.map subst_branch branches$ >>
   
module Expr = struct
let unapplist e =
  let rec unrec acc = function
    <:hcexpr< $t$ $arg$ >> -> unrec (arg::acc) t
  | t -> (t,acc)
  in unrec [] e
;
end

let rec is_value z = match Expr.unapplist z with
    (<:hcexpr< $lid:_$ >>, []) -> true
  | (<:hcexpr< function $list:_$ >>, []) -> true
  | (<:hcexpr< $longid:_$ >>, l) -> List.for_all is_value l
  | _ -> false

let eta_simple_beta = function
    <:hcexpr< function $lid:id$ -> ( $_M$ $lid:id'$ ) >> when id = id' && FVS.not_free_in id (freevars _M) -> _M
  | <:hcexpr< (function $lid:id$ -> $_M$) $_N$ >> when is_value _N -> subst [(id, _N)] _M
  | e -> e

let pa s = s |> Stream.of_string |> Grammar.Entry.parse Pa_camlp5.expr_hashcons_eoi
end ;;
