(**pp -syntax camlp5r -package camlp5.parser_quotations,camlp5.extfun,pa_ppx.deriving_plugins.params *)
(* camlp5r *)
(* pa_here.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open MLast;
open Pa_ppx_base ;
open Pa_passthru ;
open Ppxutil ;
open Pa_ppx_utils ;
open Pa_ppx_deriving ;
open Pa_ppx_params_runtime.Runtime ;

module TypeMap = struct
  type t 'a = list (ctyp * 'a) ;
  value mt = [] ;
  value assoc k l = AList.assoc ~{cmp=Reloc.eq_ctyp} k l ;
  value mem k l = AList.assoc ~{cmp=Reloc.eq_ctyp} k l ;
  value remove k l = AList.assoc ~{cmp=Reloc.eq_ctyp} k l ;
end ;

value two_level_mem_assoc k1 k2 dict =
  match List.assoc k1 dict with [
      exception Not_found -> False
    | d -> List.mem_assoc k2 d
    ]
;

value two_level_add_assoc (k1, k2, v) dict =
  match List.assoc k1 dict with [
      exception Not_found ->
        [(k1, [(k2,v)]) :: dict]
    | d ->
       let d = [(k2,v) :: (List.remove_assoc k2 d)] in
       [(k1, d) :: (List.remove_assoc k1 dict)]
    ]
;

value test_types = ref [] ;
value add_test_type s = test_types.val := [s :: test_types.val] ;

value expanded_types = ref [] ;
value add_expanded_type s = expanded_types.val := [s :: expanded_types.val] ;
value expanded_type n = List.mem n expanded_types.val ;

value extract_expansion (n,td) =
  let ty = match td.tdDef with [
        <:ctyp< $_$ == $ty$ >> -> ty
      | ty -> ty
      ] in
  let tyvars =
    td.tdPrm |> uv
    |> List.map (fun tv ->
           match uv (fst tv) with [
               None -> Fmt.(failwithf "Pa_ppx_quotation_test.extract_expansion: unnamed type-var for type-decl %s" n)
             | Some v -> v
         ]) in
  (tyvars, ty)
;

type expand_op_t = [
    Auto
  | AddDel of list expr and list expr
  | Explicit of list expr
  ] [@@deriving params;]
;

value compute_expansion_dict type_decls expand_types =
  type_decls
  |> List.filter_map (fun (n, td) ->
         match List.assoc n expand_types with [
             exception Not_found -> None
           | insn ->
              Some ((n: string), (extract_expansion (n,td), insn))
           ])
;

value compute_per_constructor_expansion_dict type_decls expand_types_per_constructor =
  expand_types_per_constructor
  |> List.map (fun (cid,  expand_types) ->
         ((cid: string), compute_expansion_dict type_decls expand_types)
       )
;

value compute_per_type_expansion_dict type_decls expand_types_per_type =
  let dict0 = expand_types_per_type in
(*
  let dict0 =
    List.fold_left
      (fun dict0 (tname, td) ->
       if Pcaml.unvala td.MLast.tdPrm <> [] then dict0 else
         if not (two_level_mem_assoc tname tname dict0) then
           two_level_add_assoc (tname, tname, Auto) dict0
         else dict0)
      dict0 type_decls in
 *)
  dict0
  |> List.map (fun (tname,  expand_types) ->
         ((tname: string), compute_expansion_dict type_decls expand_types)
       )
;

value compute_module_dict type_decls type_module_map =
  type_decls
  |> List.filter_map (fun (n, td) ->
         if match td.tdDef with [
                <:ctyp< [ $list:_$ ] >>
              | <:ctyp< $_$ == [ $list:_$ ] >>
              | <:ctyp< { $list:_$ } >>
              | <:ctyp< $_$ == { $list:_$ } >> -> False
              | _ -> True
              ] then
           None
         else if List.mem_assoc n type_module_map then
           Some(n, List.assoc n type_module_map)
         else
           let tcon = match td.tdDef with [
                 <:ctyp< $ty$ == $_$ >> -> fst (Ctyp.unapplist ty)
               | _ -> Fmt.(raise_failwithf (loc_of_type_decl td) "pa_ppx_quotation_test: neither manifest type not explicit module specified for typedecl %s" n)
               ] in
           match tcon with [
               <:ctyp< $longid:li$ . $lid:_$ >> -> Some(n, li)
             | <:ctyp< $lid:_$ >> ->
                            Fmt.(raise_failwithf (loc_of_type_decl td) "pa_ppx_quotation_test: manifest type had no module for typedecl %s" n)
             | _ -> Fmt.(raise_failwithf (loc_of_type_decl td) "pa_ppx_quotation_test: cannot infer a module for typedecl %s" n)
             ])
;
type t = {
  optional : bool [@default False;]
; plugin_name : string [@default "";]
; type_decls : list (string * type_decl) [@computed type_decls;]
; test_types : list lident
; expand_types : alist lident expand_op_t [@default [];]
; per_constructor_expansion : list (uident * expand_op_t) [@default [];]
; expansion_dict : alist lident ((list string * ctyp) * expand_op_t) [@computed compute_expansion_dict type_decls expand_types;]
; expand_types_per_constructor : list (uident * (alist lident expand_op_t)) [@default [];]
; per_constructor_expansion_dict : list (uident * (alist lident ((list string * ctyp) * expand_op_t))) [@computed compute_per_constructor_expansion_dict type_decls expand_types_per_constructor;]
; expand_types_per_type : alist lident (alist lident expand_op_t) [@default [];]
; per_type_expansion_dict : alist lident (alist lident ((list string * ctyp) * expand_op_t)) [@computed compute_per_type_expansion_dict type_decls expand_types_per_type;]
; prefix_of_type: alist ctyp lident[@default [];]
; type_module_map : alist lident longid[@default [];]
; module_dict : alist lident longid[@computed compute_module_dict type_decls type_module_map;]
; default_expression : alist lident expr[@default [];]
; location_type : option ctyp
; target_is_pattern_ast : bool [@default False;]
; minimal_record_module_labels : bool [@default False;]
; superfluous_constructors : list uident [@default [];]
; loc_varname : lident [@default "loc";]
} [@@deriving params {
         formal_args = {
       t = [ type_decls ]
     ; pertype_t = [ default_data_source_module
                   ; default_quotation_source_module ]
         }
           };]
;

value build_params loc ctxt tdl =
  let type_decls = List.map (fun (MLast.{tdNam=tdNam} as td) ->
      (tdNam |> uv |> snd |> uv, td)
    ) tdl in
  let optarg =
    let l = List.map (fun (k, e) -> (<:patt< $lid:k$ >>, e)) (Ctxt.options ctxt) in
    <:expr< { $list:l$ } >> in
  params type_decls optarg
;

value build_params_from_cmdline tdl =
  let type_decls = List.map (fun (MLast.{tdNam=tdNam} as td) ->
      (tdNam |> uv |> snd |> uv, td)
    ) tdl in
  let expand_types = expanded_types.val |> List.map (fun n -> (n, Auto)) in
  {
    optional = False
  ; plugin_name = "pa_quotation_test"
  ; type_decls = type_decls
  ; test_types = test_types.val
  ; expand_types = expand_types
  ; expand_types_per_constructor = []
  ; expand_types_per_type = []
  ; per_constructor_expansion = []
  ; expansion_dict = compute_expansion_dict type_decls expand_types
  ; per_constructor_expansion_dict = []
  ; per_type_expansion_dict = compute_per_type_expansion_dict type_decls []
  ; prefix_of_type = []
  ; type_module_map = []
  ; module_dict = compute_module_dict type_decls []
  ; default_expression = []
  ; location_type = None
  ; target_is_pattern_ast = False
  ; minimal_record_module_labels = False
  ; superfluous_constructors = []
  ; loc_varname = "loc"
  }
;

Pcaml.strict_mode.val := True;

value rec pfx rc short t =
  let t =
    match t with
    [ <:ctyp< Ploc.vala $t$ >> -> t
    | t -> t ]
  in
  if match rc.location_type with [ None -> False | Some lty -> Reloc.eq_ctyp t lty ] then
    if short then "l" else "loc"
  else
  match t with
  [ <:ctyp< bool >> -> "b"
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
  | <:ctyp< class_infos $t$ >> -> "ci" ^ pfx rc True t
  | <:ctyp< list $t$ >> -> "l" ^ pfx rc True t
  | <:ctyp< option $t$ >> -> pfx rc True t
  | <:ctyp< ($list:tl$) >> -> String.concat "" (List.map (pfx rc True) tl)
  | _ -> "x" ]
;

value prefix_of_type rc t =
  match AList.assoc ~{cmp=Reloc.eq_ctyp} t rc.prefix_of_type with [
      x -> x
    | exception Not_found -> pfx rc False t
    ]
;

value name_of_vars rc proj_t xl =
  let (rev_tnl, env) =
    List.fold_left
      (fun (rev_tnl, env) x ->
         let t = proj_t x in
         let pt = prefix_of_type rc t in
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

value handle_vala loc rc f e =
  if rc.target_is_pattern_ast then
    f <:expr< Ploc.VaVal $e$ >>
  else f e
;

value per_constructor_expand_type_p rc tname cid =
  two_level_mem_assoc cid tname rc.per_constructor_expansion_dict ;

value per_type_expand_type_p rc ~{tdname} tname =
  two_level_mem_assoc tdname tname rc.per_type_expansion_dict ;

value expand_type_p rc ~{tdname} cidopt tname =
  List.mem_assoc tname rc.expansion_dict ||
    match cidopt with [
        None -> False
      | Some cid ->
         per_constructor_expand_type_p rc tname cid
      ] ||
  per_type_expand_type_p rc ~{tdname} tname
;

value do_expand_via_dict0 dict x =
  let (x,args) = Ctyp.unapplist x in
  let tname = match x with [
        <:ctyp< $lid:tname$ >> -> tname
      | _ -> assert False
      ] in
  let ((formals, ty), insn) = List.assoc tname dict in
  if List.length args <> List.length formals then
    Fmt.(failwithf "Pa_ppx_quotation_test.do_expand_type: mismatched actuals/formals for type %s: [%a] <> [%a]"
           tname (list ~{sep=(const string " ")} Pp_MLast.pp_ctyp) args (list ~{sep=(const string " ")} string) formals)
  else
    let rho = Std.combine formals args in
    (Ctyp.subst rho ty, insn)
;

value do_expand_via_dict dict x =
  match do_expand_via_dict0 dict x with [
      exception Not_found -> None
    | x -> Some x
    ]
;

value do_expand_type0 rc x = do_expand_via_dict rc.expansion_dict x ;

value do_expand_per_constructor rc cidopt x =
  match cidopt with [
      None -> None
    | Some cid ->
       match List.assoc cid rc.per_constructor_expansion_dict with [
           exception Not_found -> None
         | dict -> do_expand_via_dict dict x
         ]
    ]
;

value do_expand_per_type rc ~{tdname} x =
  match List.assoc tdname rc.per_type_expansion_dict with [
      exception Not_found -> None
    | dict -> do_expand_via_dict dict x
    ]
;

value do_expand_type rc ~{tdname} cidopt x =
  let rv = List.find_map
          (fun f -> f x)
          [(fun x -> do_expand_per_constructor rc cidopt x);
           (fun x -> do_expand_per_type rc ~{tdname} x);
           (fun x -> do_expand_type0 rc x)] in
  match rv with [
      None -> Fmt.(failwithf "Internal error: do_expand_type: failed to expand %a" Pp_MLast.pp_ctyp x)
    | Some x -> x
    ]
;

value process_add_dels (adds,dels) l =
  let l = List.map (Reloc.expr (fun _ ->  Ploc.dummy) 0) l in
  let adds = List.map (Reloc.expr (fun _ ->  Ploc.dummy) 0) adds in
  let dels = List.map (Reloc.expr (fun _ ->  Ploc.dummy) 0) dels in
  let l = l@adds in
  let l = Std.subtract l dels in
  Std.uniquize l
;

value name_of_type rc n t =
  if n <> "" then n
  else snd (List.hd (name_of_vars rc (fun x -> x) [t]))
;

value name_of_tuple_types rc n tl =
  if n <> "" then
    List.mapi (fun i _ -> n^"f"^(string_of_int (i+1))) tl
  else List.map snd (name_of_vars rc (fun x -> x) tl)
;

value rec expr_list_of_type_gen loc rc ~{tdname} f n ((modli, cid), x) =
  expr_list_of_type_gen_uncurried rc (loc, tdname, f, n, ((modli,cid), x))
and expr_list_of_type_gen_uncurried rc (loc, tdname, f, n, ((modli,cid), x)) =
  if match rc.location_type with [
         None -> False
       | Some lty -> Reloc.eq_ctyp lty x
       ] then
    f <:expr< $lid:rc.loc_varname$ >>
  else
  match (x, Ctyp.unapplist x) with
  [ (<:ctyp< $lid:tname$ >>, _) when List.mem_assoc tname rc.default_expression ->
    let e = List.assoc tname rc.default_expression in
    [e]
  | (_, (<:ctyp< $lid:tname$ >>, _)) when expand_type_p rc ~{tdname} cid tname ->
     let modli_opt = match List.assoc tname rc.module_dict with [
           exception Not_found ->
                     None
                   | x -> Some x
         ] in
     let (expanded, insn) = do_expand_type rc ~{tdname} cid x in
     match insn with [
         Auto ->
         expr_list_of_type_gen loc rc ~{tdname} f n ((modli_opt, cid), expanded)
       | AddDel  adds dels ->
          let l = expr_list_of_type_gen loc rc ~{tdname} f n ((modli_opt, cid), expanded) in
          process_add_dels (adds,dels) l

       | Explicit l -> l
       ]

  | (<:ctyp< Ploc.vala $t$ >>, _) ->
     let n = name_of_type rc n x in
      expr_list_of_type_gen loc rc ~{tdname} (handle_vala loc rc f) n ((None, cid), t) @
      let n = add_o n t in
      f <:expr< $lid:n$ >>
  | (<:ctyp< bool >>, _) ->
      f <:expr< True >> @
      f <:expr< False >> @
      f <:expr< $lid:n$ >>

  | ((<:ctyp< { $list:_$ }>> as ct), _) -> expr_list_of_record_ctyp rc ~{tdname} f ((modli, cid), ct)

  | ((<:ctyp< [ $list:_$ ]>> as ct), _) -> expr_list_of_variant_ctyp rc ~{tdname} f (modli, ct)

  | (<:ctyp< ( $list:l$ )>>, _) -> 
     let namel = name_of_tuple_types rc n l in
     let ll = List.map2 (fun n t -> expr_list_of_type_gen loc rc ~{tdname} (fun x -> [x]) n ((None, cid), t)) namel l in
    let l = expr_list_cross_product ll in
    List.concat (List.map (fun l -> f <:expr< ( $list:l$ ) >>) l)

  | (<:ctyp< option $t$ >>, _) ->
      f <:expr< None >> @
      match t with
      [ <:ctyp< Ploc.vala (list $t$) >> ->
          let f _ = f (if rc.target_is_pattern_ast then <:expr< Some (Ploc.VaVal []) >> else <:expr< Some [] >>) in
          expr_list_of_type_gen loc rc ~{tdname} f n ((None, cid), t)
      | _ -> [] ] @
      expr_list_of_type_gen loc rc ~{tdname} (fun e -> f <:expr< Some $e$ >>) n ((None, cid), t) @
      let n = name_of_type rc n x in
      let n = add_o ("o" ^ n) t in
      f <:expr< $lid:n$ >>
  | _ ->
      let n = name_of_type rc n x in
      f <:expr< $lid:n$ >> ]

and expr_list_of_record_ctyp rc ~{tdname} (f : MLast.expr -> list MLast.expr) ((modli,cid), ty) = match ty with [
  <:ctyp:< { $list:ldl$ } >> ->
    let modli = match modli with [
          None -> Fmt.(raise_failwithf loc "expr_list_of_record_ctyp: no module supplied for type %a" Pp_MLast.pp_ctyp ty)
        | Some li -> li
        ] in
    let ldnl = name_of_vars rc (fun (loc, l, mf, t, _) -> t) ldl in
    let mk_first_pattern ((loc, l, mf, t, _), n) = <:patt< $longid:modli$ . $lid:l$ >> in
    let mk_rest_patterns ((loc, l, mf, t, _), n) =
      if rc.minimal_record_module_labels then
        <:patt< $lid:l$ >>
      else
        <:patt< $longid:modli$ . $lid:l$ >>
    in

    let pl = [mk_first_pattern (List.hd ldnl) :: List.map mk_rest_patterns (List.tl ldnl)] in
    let exprs1 ((loc, l, mf, t, _), n) = expr_list_of_type loc rc ~{tdname} (fun x -> [x]) n (cid,t) in
    let ell = ldnl |> List.map exprs1 in
    let exp_row_l = expr_list_cross_product ell in
    let pe_row_l = List.map (Std.combine pl) exp_row_l in
    let el = List.map (fun pe_row -> <:expr< {$list:pe_row$} >>) pe_row_l in
    List.concat_map f el

| ct -> Ploc.raise (MLast.loc_of_ctyp ct) (Failure "expr_list_of_record_ctyp: not a record ctyp")
]

and expr_list_of_variant_ctyp rc ~{tdname} (f : MLast.expr -> list MLast.expr) (modli, ty) = match ty with [
  <:ctyp:< [ $list:cdl$ ] >> ->
    let el = List.fold_right (fun cd el -> expr_of_cons_decl rc ~{tdname} (modli,cd) @ el) cdl [] in
    List.concat (List.map f el)
| ct -> Ploc.raise (MLast.loc_of_ctyp ct) (Failure "expr_list_of_variant_ctyp: not a variant ctyp")
]

and expr_list_of_type loc rc ~{tdname} (f : MLast.expr -> list MLast.expr) n (cid, ty) =
  expr_list_of_type_gen loc rc ~{tdname} f n ((None, cid), ty)

and patt_expr_list_of_type loc rc ~{tdname} (f : MLast.expr -> list (list (MLast.patt * MLast.expr))) n (cid, ty) =
  let el = expr_list_of_type loc rc ~{tdname} (fun x -> [x]) n (cid,ty) in
  List.concat (List.map f el)

and expr_of_cons_decl rc ~{tdname} (modli, (loc, c, x, tl, rto, y)) =
  match List.assoc (Pcaml.unvala c) rc.per_constructor_expansion with [
      exception Not_found
    | Auto -> 
       expr_of_cons_decl0 rc (tdname, modli, (loc, c, x, tl, rto, y))
    | Explicit l -> l
    | AddDel adds dels ->
       let l = expr_of_cons_decl0 rc (tdname, modli, (loc, c, x, tl, rto, y)) in
       process_add_dels (adds,dels) l
    ]

and expr_of_cons_decl0 rc (tdname, modli, (loc, c, _, tl, rto, _)) = do {
  let c = Pcaml.unvala c in
  let modli = match modli with [
        None -> Fmt.(raise_failwithf loc "expr_of_cons_decl: no module supplied for constructor %s" c)
      | Some li -> li
      ] in
  if List.mem c rc.superfluous_constructors then []
  else do {
    let tl = Pcaml.unvala tl in
    let tnl = name_of_vars rc (fun t -> t) tl in
    let exprs1 (t, tn) =
      expr_list_of_type_gen loc rc ~{tdname} (fun x -> [x]) tn ((None, Some c), t) in
    let ell = List.map exprs1 tnl in
    let el = expr_list_cross_product ell in
    let mkapp l =
      List.fold_left (fun e1 e2 -> <:expr< $e1$ $e2$ >>) <:expr< $longid:modli$ . $uid:c$ >> l in
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

value expr_list_of_type_decl loc rc td =
  let tname = Pcaml.unvala (snd (Pcaml.unvala td.MLast.tdNam)) in
  if List.mem tname rc.test_types then
    if Pcaml.unvala td.MLast.tdPrm <> [] then
      Fmt.(raise_failwithf loc "expr_list_of_type_decl: typedecl %s has params (cannot be expanded in list of examples" tname)
    else

    let ty = match td.MLast.tdDef with [
          <:ctyp< $_$ == $ty$ >> -> ty
        | ty -> ty
        ] in
    let modli_opt = match List.assoc tname rc.module_dict with [
          exception Not_found -> None
        | x -> Some x
        ] in

    if expand_type_p rc ~{tdname=tname} None tname then
      let cid = None in
      let x = <:ctyp< $lid:tname$ >> in
      let tdname = tname in
      let (expanded, insn) = do_expand_type rc ~{tdname} cid x in
      let f = (fun x -> [x]) in
      let n = "" in
      match insn with [
          Auto ->
          expr_list_of_type_gen loc rc ~{tdname} f n ((modli_opt, cid), expanded)
        | AddDel  adds dels ->
           let l = expr_list_of_type_gen loc rc ~{tdname} f n ((modli_opt, cid), expanded) in
           process_add_dels (adds,dels) l
          
        | Explicit l -> l
        ]
    else
      expr_list_of_type_gen loc rc ~{tdname=tname} (fun x -> [x]) "" ((modli_opt, None), ty)
  else []
;

value drop_duplicates el =
  let ht = Hashtbl.create 23 in
  let canon e = Reloc.expr (fun _ -> Ploc.dummy) 0 e in
  let rec drec acc = fun [
        [] -> List.rev acc
      | [h::tl] when Hashtbl.mem ht (canon h) -> drec acc tl
      | [h::tl] -> do {
          Hashtbl.add ht (canon h) () ;
          drec [h::acc] tl
        }
      ]
  in drec [] el
;

value type_decl_gen_ast loc rc td =
  let loc = loc_of_type_decl td in
  let tname = Pcaml.unvala (snd (Pcaml.unvala td.MLast.tdNam)) in
  let el = expr_list_of_type_decl loc rc td in
  let el = drop_duplicates el in
  let sil = List.map (fun e -> <:str_item< $exp:e$ >>) el in
  [<:str_item< [@@@"ocaml.text" $str:tname$; ] >> :: sil]
;

value type_decls_gen_ast loc rc tdl =
  tdl |> List.concat_map (type_decl_gen_ast loc rc)
;

value pp_str_item pps ty = Fmt.(pf pps "#<str_item< %s >>" (Eprinter.apply Pcaml.pr_str_item Pprintf.empty_pc ty)) ;

value type_decls_gen_quotation_test loc arg rc tdl =
  let sil = type_decls_gen_ast loc rc tdl in
  <:str_item< declare $list:sil$ end >>
;

value derive_quotation_test name arg = fun [
  <:str_item:< type $_flag:nrfl$ $list:tdl$ >> ->
    let rc = build_params loc arg tdl in
    type_decls_gen_quotation_test loc arg rc tdl
| si -> Fmt.(raise_failwithf (MLast.loc_of_str_item si) "pa_ppx_q_ast.quotation_test: unrecognized extension payload:\n@[%a@]"
               pp_str_item si)
]
;

value rewrite_str_item arg = fun [
  <:str_item:< [%%"quotation_test" type $list:tdl$ ;] >> as z ->
  let rc = build_params_from_cmdline tdl in
  type_decls_gen_quotation_test loc arg rc tdl
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
  Pa_passthru.(install { name = "pa_quotation_test"; ef =  ef ; pass = None ; before = [] ; after = [] })
;

install();

Pcaml.add_option "-pa_ppx_q_ast.quotation_test-test-type" (Arg.String add_test_type)
  "test specified type (generate patterns for it)";

Pcaml.add_option "-pa_ppx_q_ast.quotation_test-expand-type" (Arg.String add_expanded_type)
  "expand specified type";

Pa_deriving.(Registry.add PI.{
  name = "quotation_test"
; alternates = []
; options = [
    "optional"
  ; "test_types"
  ; "expand_types"
  ; "expand_types_per_constructor"
  ; "expand_types_per_type"
  ; "per_constructor_expansion"
  ; "prefix_of_type"
  ; "type_module_map"
  ; "default_expression"
  ; "location_type"
  ; "target_is_pattern_ast"
  ; "minimal_record_module_labels"
  ; "superfluous_constructors"
  ; "loc_varname"
  ]
; default_options = let loc = Ploc.dummy in [
    ("optional", <:expr< False >>)
  ; ("expand_types", <:expr< () >>)
  ; ("expand_types_per_constructor", <:expr< [] >>)
  ; ("expand_types_per_type", <:expr< () >>)
  ; ("per_constructor_expansion", <:expr< [] >>)
  ; ("prefix_of_type", <:expr< () >>)
  ; ("type_module_map", <:expr< () >>)
  ; ("default_expression", <:expr< () >>)
  ; ("target_is_pattern_ast", <:expr< False >>)
  ; ("minimal_record_module_labels", <:expr< False >>)
  ; ("superfluous_constructors", <:expr< [] >>)
  ; ("loc_varname", <:expr< loc >>)
  ]
; alg_attributes = []
; expr_extensions = []
; ctyp_extensions = []
; expr = (fun arg e -> assert False)
; ctyp = (fun arg e -> assert False)
; str_item = derive_quotation_test
; sig_item = (fun arg e -> assert False)
})
;

