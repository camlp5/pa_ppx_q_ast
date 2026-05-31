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

value pp_str_item pps ty = Fmt.(pf pps "#<str_item< %s >>" (Eprinter.apply Pcaml.pr_str_item Pprintf.empty_pc ty)) ;

value pp_ctyp pps ty = Fmt.(pf pps "#<ctyp< %s >>" (Eprinter.apply Pcaml.pr_ctyp Pprintf.empty_pc ty)) ;
value pp_expr pps ty = Fmt.(pf pps "#<expr< %s >>" (Eprinter.apply Pcaml.pr_expr Pprintf.empty_pc ty)) ;

value pp_patt pps ty = Fmt.(pf pps "#<patt< %s >>" (Eprinter.apply Pcaml.pr_patt Pprintf.empty_pc ty)) ;

module TypeMap = struct
  type t 'a = list (ctyp * 'a) ;
  value mt = [] ;
  value assoc k l = AList.assoc ~{cmp=Reloc.eq_ctyp} k l ;
  value assoc_opt k l =
    match assoc k l with [
        exception Not_found -> None
      | x -> Some x
      ]
  ;
  value mem k l = AList.mem ~{cmp=Reloc.eq_ctyp} k l ;
  value remove k l = AList.remove ~{cmp=Reloc.eq_ctyp} k l ;

value two_level_mem_assoc k1 k2 dict =
  match List.assoc k1 dict with [
      exception Not_found -> False
    | d -> mem k2 d
    ]
;
  
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

module Raw = struct
type expand_op_t = [
    Auto
  | AddDel of list expr and list expr
  | DelPatts of list patt
  | Explicit of list expr
  ] [@@deriving (params, show);]
;
end ;

module Cooked = struct
type expand_op_t = [
    Expand of list string and ctyp
  | ExpandTo of ctyp
  | Explicit of list expr
  | AddDel of list expr and list expr
  | DelPatts of list patt
  ] [@@deriving params;]
;

value rec compute_expansion1 stk type_decls (ty,insn) =
  match (Ctyp.unapplist ty, insn) with [
      ((<:ctyp:< $lid:n$ >>, []), Raw.Auto) ->
        match List.assoc n type_decls with [
            exception Not_found ->
                      Fmt.(raise_failwithf loc "compute_expansion: cannot find typedecl for %s" n)
                    | td ->
                       let (tyvars, tk) = extract_expansion (n, td) in
                       [Expand tyvars tk]
          ]

    | (((<:ctyp< option >> | <:ctyp< list >> | <:ctyp< Ploc.vala >>),
        _), Raw.AddDel adds dels) ->
       [AddDel adds dels]
    | (_, Raw.AddDel adds dels) ->
       let insns = compute_expansion1 [(ty,insn) :: stk] type_decls (ty, Raw.Auto) in
       insns@[AddDel adds dels]

    | (((<:ctyp< option >> | <:ctyp< list >> | <:ctyp< Ploc.vala >>),
        _), Raw.DelPatts dels) ->
       [DelPatts dels]
    | (_, Raw.DelPatts dels) ->
       let insns = compute_expansion1 [(ty,insn) :: stk] type_decls (ty, Raw.Auto) in
       insns@[DelPatts dels]

    | (_, Raw.Explicit el) ->
       [Explicit el]

    | _ -> do {
        Fmt.(pf stderr "compute_expansion: internal error (unhandled case): stack is:@.%a@."
               (list (pair pp_ctyp Raw.pp_expand_op_t)) stk
        ) ;
        Fmt.(raise_failwithf (MLast.loc_of_ctyp ty) "compute_expansion: unhandled case ty=%a insn=<<%a>>"
               pp_ctyp ty
               Raw.pp_expand_op_t insn
        )
      }
    ]
;

value compute_expansion type_decls (ty,insn) =
  do {
    Fmt.(pf stderr "compute_expansion: %a@."
           (pair pp_ctyp Raw.pp_expand_op_t) (ty, insn)
        ) ;
  compute_expansion1 [] type_decls (ty,insn)
  }
;

end ;

value compute_expansion_dict type_decls expand_types =
  expand_types
  |> List.map (fun (ty, insn) -> (ty,  Cooked.compute_expansion type_decls (ty, insn)))
;

value compute_per_constructor_expansion_dict type_decls expand_types_per_constructor =
  expand_types_per_constructor
  |> List.map (fun (cid,  expand_types) ->
         ((cid: string), compute_expansion_dict type_decls expand_types)
       )
;

value compute_per_type_expansion_dict type_decls expand_types_per_type =
  let dict0 = expand_types_per_type in
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
; per_constructor_expansion : list (uident * Raw.expand_op_t) [@default [];]
; expand_types : list (ctyp *  Raw.expand_op_t) [@default [];]
; expansion_dict : alist ctyp (list Cooked.expand_op_t) [@computed compute_expansion_dict type_decls expand_types;]
; expand_types_per_constructor : list (uident * (list (ctyp * Raw.expand_op_t))) [@default [];]
; per_constructor_expansion_dict : list (uident * (alist ctyp (list Cooked.expand_op_t))) [@computed compute_per_constructor_expansion_dict type_decls expand_types_per_constructor;]
; expand_types_per_type : alist lident (list (ctyp * Raw.expand_op_t)) [@default [];]
; per_type_expansion_dict : alist lident (alist ctyp (list Cooked.expand_op_t)) [@computed compute_per_type_expansion_dict type_decls expand_types_per_type;]
; prefix_of_type: alist ctyp lident[@default [];]
; type_module_map : alist lident longid[@default [];]
; module_dict : alist lident longid[@computed compute_module_dict type_decls type_module_map;]
; default_expression : alist lident expr[@default [];]
; location_type : option ctyp
; target_is_pattern_ast : bool [@default False;]
; minimal_record_module_labels : bool [@default False;]
; superfluous_constructors : list uident [@default [];]
; loc_varname : lident [@default "loc";]
; varname : lident [@default "x";]
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
  let loc = Ploc.dummy in
  let expand_types = expanded_types.val |> List.map (fun n -> (<:ctyp< n >>, Raw.Auto)) in
  {
    optional = False
  ; plugin_name = "pa_quotation_test"
  ; type_decls = type_decls
  ; test_types = test_types.val
  ; per_constructor_expansion = []
  ; expand_types = expand_types
  ; expand_types_per_constructor = []
  ; expand_types_per_type = []
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
  ; varname = "x"
  }
;

Pcaml.strict_mode.val := True;

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

value handle_vala loc rc el =
  let el = el @ [<:expr< $lid:rc.varname$ >>] in
  if rc.target_is_pattern_ast then
    (List.map (fun e -> <:expr< Ploc.VaVal $e$ >>) el) @
      [<:expr< $lid:rc.varname$ >>]
  else el
;

value do_expand_via_dict0 dict ty =
  let (rootty,args) = Ctyp.unapplist ty in
  let (tyargs, insns) = match (TypeMap.assoc_opt ty dict, TypeMap.assoc_opt rootty dict) with [
      (None, None) ->
      Fmt.(failwithf "do_expand_via_dict0: type %a not found in dict" Pp_MLast.pp_ctyp ty)
    | (Some _, Some _) when args <> [] ->
       Fmt.(failwithf "do_expand_via_dict0: type %a has ambiguous expansion in dict" Pp_MLast.pp_ctyp ty)
    | (Some insns, _) ->
       ((ty, []), insns)
    | (None, Some insns) ->
       ((rootty, args), insns)
      ] in

  match insns with [
      [Cooked.Expand formals rhs :: tl] ->
      if List.length args <> List.length formals then
        Fmt.(failwithf "Pa_ppx_quotation_test.do_expand_type: mismatched actuals/formals for type %a: [%a] <> [%a]"
               Pp_MLast.pp_ctyp ty
               (list ~{sep=(const string " ")} Pp_MLast.pp_ctyp) args
               (list ~{sep=(const string " ")} string) formals)
      else
        let rho = Std.combine formals args in
        let rhs = Ctyp.subst rho rhs in
        (tyargs, [Cooked.ExpandTo rhs :: tl])
    | insns -> (tyargs, insns)
    ]
;

value can_expand_via_dict0 dict ty =
  TypeMap.mem ty dict || TypeMap.mem (fst (Ctyp.unapplist ty)) dict
;

value per_constructor_expand_type_p rc ~{cid} ty  =
  List.mem_assoc cid rc.per_constructor_expansion_dict &&
    can_expand_via_dict0 (List.assoc cid rc.per_constructor_expansion_dict) ty
;

value per_type_expand_type_p rc ~{tdname} ty =
  List.mem_assoc tdname rc.per_type_expansion_dict &&
    can_expand_via_dict0 (List.assoc tdname rc.per_type_expansion_dict) ty
;

value expand_type_p rc ~{tdname} cidopt ty =
  can_expand_via_dict0 rc.expansion_dict ty ||
  match cidopt with [ None -> False
                    | Some cid -> per_constructor_expand_type_p rc ~{cid=cid} ty ] ||
  (per_type_expand_type_p rc ~{tdname} ty)
;

value do_expand_via_dict dict x =
  match do_expand_via_dict0 dict x with [
      exception Failure _ -> None
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
      None -> ((x, []), [])
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

value extract_field lel plab =
  lel |> List.find_opt (fun (elab, e) -> Reloc.eq_patt plab elab) |> Option.map snd
;

value rec pattmatch1 patt e =
  match (patt, e) with [
      (<:patt< _ >>, _) -> True
    | (<:patt< $lid:_$ >>, <:expr< $lid:_$ >>) -> True
    | (<:patt< ( $p1$ | $p2$ ) >>, e) ->
       pattmatch1 p1 e || pattmatch1 p2 e
    | (<:patt:< $longid:pli$ >>, <:expr< $longid:eli$ >>) -> Reloc.eq_longid pli eli
    | (<:patt< $_$ $_$  >>, <:expr< $_$ $_$ >>) ->
       let (pf, pargs) = Patt.unapplist patt in
       let (f, args) = Expr.unapplist e in
       List.length pargs = List.length args &&
         (match (pf, f) with [
              (<:patt:< $longid:pli$ >>, <:expr< $longid:eli$ >>)
                   when Reloc.eq_longid pli eli -> True
            | _ -> False
         ]) &&
           List.for_all2 pattmatch1 pargs args
    | (<:patt< ( $list:pl$ ) >>, <:expr< ( $list:el$ ) >>) ->
       List.length pl = List.length el &&
         List.for_all2 labeled_pattmatch pl el

    | (<:patt< { $list:lpl$ } >>, <:expr< { $list:lel$ } >>) ->
       let lpl = List.map (fun (p1,p2) -> (Reloc.patt (fun _ -> Ploc.dummy) 0 p1,
                                           Reloc.patt (fun _ -> Ploc.dummy) 0 p2)) lpl in
       let lel = List.map (fun (p,e) -> (Reloc.patt (fun _ -> Ploc.dummy) 0 p,
                                         Reloc.expr (fun _ -> Ploc.dummy) 0 e)) lel in
       lpl |> List.for_all (fun (plab,p) ->
                  match extract_field lel plab with [
                      None -> False
                    | Some e -> pattmatch1 p e
                    ])

    | _ -> False
    ]
and labeled_pattmatch p e =
  let (plab, p) =
    match p with [
        <:patt< ~{$lid:n$ = $p$} >> -> (Some n,p)
      | <:patt< ~{$lid:n$} >> -> (Some n, p)
      | _ -> (None, p)
      ] in
  let (elab, e) =
    match e with [
        <:expr< ~{$lid:n$ = $e$} >> -> (Some n, e)
      | <:expr:< ~{$lid:n$} >> -> (Some n, <:expr< $lid:n$ >>)
      | _ -> (None, e)
      ] in
  plab = elab && pattmatch1 p e
;

value pattmatch p e = pattmatch1 p e ;

value pattern_matches patts e =
  List.exists (fun p -> pattmatch p e) patts
;

value except pred l =
  List.filter (fun x -> not(pred x)) l
;

value process_del_patts patts l =
  let patts = List.map (Reloc.patt (fun _ -> Ploc.dummy) 0) patts in
  let l = List.map (Reloc.expr (fun _ -> Ploc.dummy) 0) l in
  except (pattern_matches patts) l
;

value apply_expand_instructions insns el =
  List.fold_left (fun el -> fun [
      Cooked.Explicit el -> el
    | AddDel adds dels -> process_add_dels (adds,dels) el
    | DelPatts dels -> process_del_patts dels el
    ]) el insns 
;

value rec expr_list_of_type_gen loc rc ~{tdname} ((modli, cid), x) =
  expr_list_of_type_gen_uncurried rc (loc, tdname, ((modli,cid), x))
and expr_list_of_type_gen_uncurried rc (loc, tdname, ((modli,cid), x)) =
  if match rc.location_type with [
         None -> False
       | Some lty -> Reloc.eq_ctyp lty x
       ] then
    [<:expr< $lid:rc.loc_varname$ >>]
  else
    let ((rootty, args), insns) =
      if expand_type_p rc ~{tdname} cid x then
        do_expand_type rc ~{tdname} cid x
      else (Ctyp.unapplist x, []) in
  match ((rootty, args), insns) with
  [ ((<:ctyp< $lid:tname$ >>, _), _) when List.mem_assoc tname rc.default_expression ->
    let e = List.assoc tname rc.default_expression in
    [e]

  | (_, [Cooked.Explicit _ :: _]) ->
     apply_expand_instructions insns []

  | (_, [Cooked.ExpandTo expanded :: tl]) ->
     let modli_opt = match rootty with [
           <:ctyp< $lid:tname$ >> when List.mem_assoc tname rc.module_dict ->
             Some(List.assoc tname rc.module_dict)
         | _ -> None
         ] in
     let el = expr_list_of_type_gen loc rc ~{tdname} ((modli_opt, cid), expanded) in
     apply_expand_instructions tl el

  | (((<:ctyp< Ploc.vala >>, [t]) | (<:ctyp< Ploc.vala $t$ >>, [])),
     insns) ->
     let el = expr_list_of_type_gen loc rc ~{tdname} ((None, cid), t) in
     let el = handle_vala loc rc el in
     apply_expand_instructions insns el

  | ((<:ctyp< bool >>, _), insns) ->
     apply_expand_instructions insns
       [<:expr< True >>
       ; <:expr< False >>
       ; <:expr< $lid:rc.varname$ >>]

  | (((<:ctyp< { $list:_$ }>> as ct), _), insns) ->
     let el = expr_list_of_record_ctyp rc ~{tdname} ((modli, cid), ct) in
     apply_expand_instructions insns el

  | (((<:ctyp< [ $list:_$ ]>> as ct), _), insns) ->
     let el = expr_list_of_variant_ctyp rc ~{tdname} (modli, ct) in
     apply_expand_instructions insns el

  | ((<:ctyp< ( $list:l$ )>>, _), insns) -> 
     let ll = List.map (fun (lab, t) ->
         let el = expr_list_of_type_gen loc rc ~{tdname} ((None, cid), t) in
         match uv lab with [
             None -> el
           | Some <:vala< lab >> ->
              List.map (fun e -> <:expr< ~{$lid:lab$ = $e$} >>) el
                ]) l in
    let ll = expr_list_cross_product ll in
    let el = List.map (fun l -> <:expr< ( $list:l$ ) >>) ll in
    apply_expand_instructions insns el

  | (((<:ctyp< option >>, [t]) | (<:ctyp< option $t$ >>, [])),
     insns) ->
     let el =
       [<:expr< None >>] @
         match t with
           [ <:ctyp< Ploc.vala (list $t$) >> ->
             if rc.target_is_pattern_ast then
               [<:expr< Some (Ploc.VaVal []) >>]
             else
               [<:expr< Some [] >>]
           | _ -> [] ] @
             (let el = expr_list_of_type_gen loc rc ~{tdname} ((None, cid), t) in
              let el = List.map (fun e -> <:expr< Some $e$ >>) el in
              el)
     in
     apply_expand_instructions insns el

  | _ ->
      [<:expr< $lid:rc.varname$ >>] ]

and expr_list_of_record_ctyp rc ~{tdname} ((modli,cid), ty) = match ty with [
  <:ctyp:< { $list:ldl$ } >> ->
    let modli = match modli with [
          None -> Fmt.(raise_failwithf loc "expr_list_of_record_ctyp: no module supplied for type %a" Pp_MLast.pp_ctyp ty)
        | Some li -> li
        ] in
    let mk_first_pattern (loc, l, mf, t, _) = <:patt< $longid:modli$ . $lid:l$ >> in
    let mk_rest_patterns (loc, l, mf, t, _) =
      if rc.minimal_record_module_labels then
        <:patt< $lid:l$ >>
      else
        <:patt< $longid:modli$ . $lid:l$ >>
    in

    let pl = [mk_first_pattern (List.hd ldl) :: List.map mk_rest_patterns (List.tl ldl)] in
    let exprs1 (loc, l, mf, t, _) = expr_list_of_type loc rc ~{tdname} (cid,t) in
    let ell = List.map exprs1 ldl in
    let exp_row_l = expr_list_cross_product ell in
    let pe_row_l = List.map (Std.combine pl) exp_row_l in
    let el = List.map (fun pe_row -> <:expr< {$list:pe_row$} >>) pe_row_l in
    el

| ct -> Ploc.raise (MLast.loc_of_ctyp ct) (Failure "expr_list_of_record_ctyp: not a record ctyp")
]

and expr_list_of_variant_ctyp rc ~{tdname} (modli, ty) = match ty with [
  <:ctyp:< [ $list:cdl$ ] >> ->
    List.fold_right (fun cd el -> expr_of_cons_decl rc ~{tdname} (modli,cd) @ el) cdl []

| ct -> Ploc.raise (MLast.loc_of_ctyp ct) (Failure "expr_list_of_variant_ctyp: not a variant ctyp")
]

and expr_list_of_type loc rc ~{tdname} (cid, ty) =
  expr_list_of_type_gen loc rc ~{tdname} ((None, cid), ty)

and expr_of_cons_decl rc ~{tdname} (modli, (loc, c, x, tl, rto, y)) =
  match List.assoc (Pcaml.unvala c) rc.per_constructor_expansion with [
      exception Not_found
    | Auto -> 
       expr_of_cons_decl0 rc (tdname, modli, (loc, c, x, tl, rto, y))
    | Explicit l -> l
    | AddDel adds dels ->
       let l = expr_of_cons_decl0 rc (tdname, modli, (loc, c, x, tl, rto, y)) in
       process_add_dels (adds,dels) l
    | DelPatts dels ->
       let l = expr_of_cons_decl0 rc (tdname, modli, (loc, c, x, tl, rto, y)) in
       process_del_patts dels l
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
    let exprs1 t =
      match t with [
          <:ctyp< { $list:_$ } >> -> 
          let el = expr_list_of_type_gen loc rc ~{tdname} ((Some modli, Some c), t) in
          el
          |> List.map (fun [
            <:expr:< { $list:pel$ } >> ->
              let pel = pel |> List.map (fun [
                  (<:patt:< $longid:_$ . $lid:id$ >>, e) -> (<:patt< $lid:id$ >>, e)
                | (<:patt< $lid:_$ >>, _) as pe -> pe
                | _ -> assert False
                ]) in
              <:expr:< { $list:pel$ } >>
          | _ -> assert False
               ])
        | _ ->
           expr_list_of_type_gen loc rc ~{tdname} ((None, Some c), t)
        ]
    in
    let ell = List.map exprs1 tl in
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
  let tdname = tname in
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

    let x = <:ctyp< $lid:tname$ >> in
    let (_, insns) = do_expand_type rc ~{tdname} None x in
    let cid = None in
    let tdname = tname in
    match insns with [
        [ExpandTo expanded :: tl] ->
        let el = expr_list_of_type_gen loc rc ~{tdname} ((modli_opt, cid), expanded) in
        Some (apply_expand_instructions tl el)
      | [_::_] ->
         Some (apply_expand_instructions insns [])

      | [] ->
         Some (expr_list_of_type_gen loc rc ~{tdname=tname} ((modli_opt, None), ty))
      ]
  else None
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
  match expr_list_of_type_decl loc rc td with [
      None -> []
    | Some el ->
       let el = drop_duplicates el in
       let sil = List.map (fun e -> <:str_item< $exp:e$ >>) el in
       [<:str_item< [@@@"ocaml.text" $str:tname$; ] >>
       ; <:str_item< [@@@"pa_ppx_q_ast.test_renumber.params"
                           { varname = $str:rc.varname$ ; loc_varname = $str:rc.loc_varname$ }; ] >>
           :: sil]
    ]
;

value type_decls_gen_ast loc rc tdl =
  tdl |> List.concat_map (type_decl_gen_ast loc rc)
;

value type_decls_gen_quotation_test loc arg rc tdl =
  let sil = type_decls_gen_ast loc rc tdl in
  <:str_item< declare $list:sil$ end >>
;

value derive_quotation_test name arg = fun [
  <:str_item:< type $_flag:nrfl$ $list:tdl$ >> ->
    let rc = build_params loc arg tdl in
    type_decls_gen_quotation_test loc arg rc tdl
| si -> Fmt.(raise_failwithf (MLast.loc_of_str_item si) "pa_ppx_q_ast.quotation_test: only typedecl is legal here:\n@[%a@]"
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
  ; "varname"
  ]
; default_options = let loc = Ploc.dummy in [
    ("optional", <:expr< False >>)
  ; ("expand_types", <:expr< [] >>)
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
  ; ("varname", <:expr< x >>)
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

