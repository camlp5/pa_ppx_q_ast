(**pp -syntax camlp5r -package camlp5.parser_quotations,camlp5.extfun *)
(* camlp5r *)
(* pa_here.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Pa_ppx_base ;
open Pa_passthru ;
open Ppxutil ;
open Pa_ppx_deriving ;

value ignored_types = ref [] ;
value add_ignored_type s = ignored_types.val := [s :: ignored_types.val] ;

value expanded_types = ref [] ;
value add_expanded_type s = expanded_types.val := [s :: expanded_types.val] ;
value expanded_type n = List.mem n expanded_types.val ;

value expansion_dict = ref [] ;
value add_expansion n v = expansion_dict.val := [(n,v) :: expansion_dict.val] ;
value expand_type n = List.assoc n expansion_dict.val ;
value has_expansion n = List.mem_assoc n expansion_dict.val ;

Pcaml.strict_mode.val := True;

value rec pfx short t =
  let t =
    match t with
    [ <:ctyp< Ploc.vala $t$ >> -> t
    | t -> t ]
  in
  match t with
  [ <:ctyp< loc >> -> if short then "l" else "loc"
  | <:ctyp< bool >> -> "b"
  | <:ctyp< class_expr >> -> "ce"
  | <:ctyp< class_sig_item >> -> "csi"
  | <:ctyp< class_str_item >> -> "csi"
  | <:ctyp< class_type >> -> "ct"
  | <:ctyp< expr >> -> "e"
  | <:ctyp< module_expr >> -> "me"
  | <:ctyp< module_type >> -> "mt"
  | <:ctyp< patt >> -> "p"
  | <:ctyp< poly_variant >> -> "pv"
  | <:ctyp< sig_item >> -> "si"
  | <:ctyp< str_item >> -> "si"
  | <:ctyp< string >> -> "s"
  | <:ctyp< ctyp >> -> "t"
  | <:ctyp< type_decl >> -> "td"
  | <:ctyp< type_var >> -> "tv"
  | <:ctyp< with_constr >> -> "wc"
  | <:ctyp< class_infos $t$ >> -> "ci" ^ pfx True t
  | <:ctyp< list $t$ >> -> "l" ^ pfx True t
  | <:ctyp< option $t$ >> -> pfx True t
  | <:ctyp< ($list:tl$) >> -> String.concat "" (List.map (pfx True) tl)
  | _ -> "x" ]
;

value prefix_of_type = pfx False;

value name_of_vars proj_t xl =
  let (rev_tnl, env) =
    List.fold_left
      (fun (rev_tnl, env) x ->
         let t = proj_t x in
         let pt = prefix_of_type t in
         let (n, env) =
           loop env where rec loop =
             fun
             [ [(n1, cnt1) :: env] ->
                 if n1 = pt then (cnt1, [(n1, cnt1 + 1) :: env])
                 else
                   let (n, env) = loop env in
                   (n, [(n1, cnt1) :: env])
             | [] -> (1, [(pt, 2)]) ]
         in
         ([(x, (pt, n)) :: rev_tnl], env))
       ([], []) xl
  in
  List.rev_map
    (fun (x, (pt, n)) ->
       let name =
         if List.assoc pt env = 2 then pt
         else pt ^ string_of_int n
       in
       (x, name))
    rev_tnl
;

value rec add_o n =
  fun
  [ <:ctyp< Ploc.vala $t$ >> -> add_o n t
  | <:ctyp< option $t$ >> -> add_o ("o" ^ n) t
  | _ -> n ]
;

value cross_product ll =
  let acc = ref [] in
  let rec crec pfx = fun [
    [] -> acc.val := [(List.rev pfx) :: acc.val]
  | [hl :: t] ->
      List.iter (fun h -> crec [h::pfx] t) hl
  ] in do {
    crec [] ll ;
    List.rev acc.val
  }
;

value expr_list_cross_product (ll : list (list MLast.expr)) = cross_product ll ;

value rec expr_list_of_type_gen loc f n x =
  expr_list_of_type_gen_uncurried (loc, f, n, x)
and expr_list_of_type_gen_uncurried (loc, f, n, x) =
  match x with
  [ <:ctyp< $lid:tname$ >> when has_expansion tname ->
    expr_list_of_type_gen loc f n (expand_type tname)
  | <:ctyp< Ploc.vala $t$ >> ->
      expr_list_of_type_gen loc (fun e -> f <:expr< Ploc.VaVal $e$ >>) n t @
      let n = add_o n t in
      f <:expr< $lid:n$ >>
  | <:ctyp< bool >> ->
      f <:expr< True >> @
      f <:expr< False >> @
      f <:expr< $lid:n$ >>
  | <:ctyp< loc >> ->
      f <:expr< loc >>

  | <:ctyp< { $list:_$ }>> as ct -> expr_list_of_record_ctyp f ct

  | <:ctyp< [ $list:_$ ]>> as ct -> expr_list_of_variant_ctyp f ct

  | <:ctyp< ( $list:l$ )>> -> 
    let ll = List.mapi (fun i t -> expr_list_of_type_gen loc (fun x -> [x]) (n^"f"^(string_of_int (i+1))) t) l in
    let l = expr_list_cross_product ll in
    List.concat (List.map (fun l -> f <:expr< ( $list:l$ ) >>) l)

  | <:ctyp< option $t$ >> ->
      f <:expr< None >> @
      match t with
      [ <:ctyp< Ploc.vala (list $t$) >> ->
          let f _ = f <:expr< Some (Ploc.VaVal []) >> in
          expr_list_of_type_gen loc f n t
      | _ -> [] ] @
      expr_list_of_type_gen loc (fun e -> f <:expr< Some $e$ >>) n t @
      let n = add_o ("o" ^ n) t in
      f <:expr< $lid:n$ >>
  | <:ctyp< override_flag >> ->
      f <:expr< MLast.Fresh >> @
      f <:expr< MLast.Override >>
  | _ ->
      f <:expr< $lid:n$ >> ]

and expr_list_of_record_ctyp (f : MLast.expr -> list MLast.expr) = fun [
  <:ctyp:< { $list:ldl$ } >> ->
    let ldnl = name_of_vars (fun (loc, l, mf, t, _) -> t) ldl in
    let pell =
      loop ldnl where rec loop =
      fun
        [ [((loc, l, mf, t, _), n) :: ldnl] ->
          let p = <:patt< MLast . $lid:l$ >> in
          let pell = loop ldnl in
          let f e = List.map (fun pel -> [(p, e) :: pel]) pell in
          patt_expr_list_of_type loc f n t
        | [] -> [[]] ]
    in
    List.concat (List.map (fun pel -> f <:expr< {$list:pel$} >>) pell)
| ct -> Ploc.raise (MLast.loc_of_ctyp ct) (Failure "expr_list_of_record_ctyp: not a record ctyp")
]

and expr_list_of_variant_ctyp (f : MLast.expr -> list MLast.expr) = fun [
  <:ctyp:< [ $list:cdl$ ] >> ->
    let el = List.fold_right (fun cd el -> expr_of_cons_decl cd @ el) cdl [] in
    List.concat (List.map f el)
| ct -> Ploc.raise (MLast.loc_of_ctyp ct) (Failure "expr_list_of_variant_ctyp: not a variant ctyp")
]

and expr_list_of_type loc (f : MLast.expr -> list MLast.expr) n ty =
  expr_list_of_type_gen loc f n ty

and patt_expr_list_of_type loc (f : MLast.expr -> list (list (MLast.patt * MLast.expr))) n ty =
  let el = expr_list_of_type loc (fun x -> [x]) n ty in
  List.concat (List.map f el)

and expr_of_cons_decl (loc, c, _, tl, rto, _) = do {
  let c = Pcaml.unvala c in
  if String.length c = 5 && String.sub c 2 3 = "Xtr" then []
  else do {
    let tl = Pcaml.unvala tl in
    let tnl = name_of_vars (fun t -> t) tl in
    let exprs1 (t, tn) =
      expr_list_of_type_gen loc (fun x -> [x]) tn t in
    let ell = List.map exprs1 tnl in
    let el = expr_list_cross_product ell in
    let mkapp l =
      List.fold_left (fun e1 e2 -> <:expr< $e1$ $e2$ >>) <:expr< MLast . $uid:c$ >> l in
    let el = List.map mkapp el in
    match c with
    [ "ExInt" | "PaInt" ->
        List.fold_right
          (fun int_type gel ->
             List.rev_append
               (List.rev_map
                  (fun e ->
                     match e with
                     [ <:expr:< $e$ s2 >> -> <:expr< $e$ $str:int_type$ >>
                     | x -> x ])
                  el)
               gel)
          [""; "l"; "L"; "n"] []
    | "PvTag" ->
        List.fold_right
          (fun e el ->
             let el = [e :: el] in
             let el =
               match e with
               [ <:expr< $f$ (Ploc.VaVal True) (Ploc.VaVal $_$) >> ->
                   [<:expr< $f$ (Ploc.VaVal True) (Ploc.VaVal []) >> :: el]
               | _ -> el ]
             in
             el)
          el []
    | _ -> el ]
  }
};

value expr_list_of_type_decl loc td =
  let tname = Pcaml.unvala (snd (Pcaml.unvala td.MLast.tdNam)) in
  if not (List.mem tname ignored_types.val) then
    let ty = match td.MLast.tdDef with [
          <:ctyp< $_$ == $ty$ >> -> ty
        | ty -> ty
        ] in
    match ty with
      [ <:ctyp< [ $list:cdl$ ] >> ->
        List.fold_right (fun cd el -> expr_of_cons_decl cd @ el) cdl []
      | <:ctyp< { $list:ldl$ } >> ->
        let ldnl = name_of_vars (fun (loc, l, mf, t, _) -> t) ldl in
        let pell =
          loop ldnl where rec loop =
          fun
            [ [((loc, l, mf, t, _), n) :: ldnl] ->
              let p = <:patt< MLast . $lid:l$ >> in
              let pell = loop ldnl in
              let f e = List.map (fun pel -> [(p, e) :: pel]) pell in
              patt_expr_list_of_type loc f n t
            | [] -> [[]] ]
        in
        List.map (fun pel -> <:expr< {$list:pel$} >>) pell
      | <:ctyp< ( $list:tl$ ) >> ->
        let nl = name_of_vars (fun t -> t) tl in
        let ell = List.map (fun (t,n) -> expr_list_of_type_gen loc (fun x -> [x]) n t) nl in
        let el = expr_list_cross_product ell in
        List.map (fun l -> <:expr< ( $list:l$ ) >>) el

      | _ -> [] ]
  else []
;

value type_decls_gen_ast loc tdl =
  let ell = List.map (fun td -> expr_list_of_type_decl loc td) tdl in
  let el = List.flatten ell in
  List.map (fun e -> <:str_item< $exp:e$ >>) el
;

value str_item_gen_quotation_test name arg = fun [
  <:str_item:< type $_flag:nrfl$ $list:tdl$ >> ->
    let sil = type_decls_gen_ast loc tdl in
    <:str_item< declare $list:sil$ end >>
| si -> Fmt.(raise_failwithf (MLast.loc_of_str_item si) "pa_ppx_q_ast.mktest: unrecognized extension payload:\n@[%a@]" Pp_MLast.pp_str_item si)
]
;

value rewrite_str_item arg = fun [
  <:str_item:< [%%"quotation_test" $stri:si$ ;] >> as z ->
  str_item_gen_quotation_test "mktest" arg si
| z -> z
]
;

value install () = 
let ef = EF.mk () in 
let ef = EF.{ (ef) with
            str_item = extfun ef.str_item with [
                <:str_item:< [%%quotation_test $stri:si$ ;] >> as z ->
    fun arg fallback ->
      Some (rewrite_str_item arg z)
  ] } in
  Pa_passthru.(install { name = "pa_mkast"; ef =  ef ; pass = None ; before = [] ; after = [] })
;

install();

Pcaml.add_option "-pa_ppx_q_ast.mktest-ignore-type" (Arg.String add_ignored_type)
  "ignore specified type";

Pcaml.add_option "-pa_ppx_q_ast.mktest-expand-type" (Arg.String add_expanded_type)
  "expand specified type";


Pa_deriving.(Registry.add PI.{
  name = "quotation_test"
; alternates = []
; options = [
    "optional"
  ]
; default_options = let loc = Ploc.dummy in [
    ("optional", <:expr< False >>)
  ]
; alg_attributes = []
; expr_extensions = []
; ctyp_extensions = []
; expr = (fun arg e -> assert False)
; ctyp = (fun arg e -> assert False)
; str_item = str_item_gen_quotation_test
; sig_item = (fun arg e -> assert False)
})
;

