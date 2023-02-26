(** -syntax camlp5r *)
(* camlp5r *)
(* pa_deriving_q_ast.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Asttools;
open MLast;
open Pa_ppx_base ;
open Pa_passthru ;
open Ppxutil ;
open Pa_ppx_deriving ;
open Surveil ;
open Pa_deriving_base ;
open Pa_ppx_utils ;
open Pa_ppx_params_runtime.Runtime ;

value debug = Pa_passthru.debug ;

value canon_ctyp ty = Reloc.ctyp (fun _ -> Ploc.dummy) 0 ty ;

value expr_as_expr loc s =
  Pcaml.handle_expr_quotation loc ("expr@", s)
;

value patt_as_expr loc s =
  Pcaml.handle_expr_quotation loc ("patt@", s)
;

module QAST = struct

value extract_case_branches = fun [
  None -> []
| Some <:expr< fun [ $list:l$ ] >> ->
  List.map (fun (p,wheno,e) ->
      match Patt.unapplist p with [
        (<:patt< $uid:uid$ >>, _) -> (uid, (p, wheno, e))
      | _ -> Ploc.raise (loc_of_patt p) (Failure "extract_case_branches: case-branches must start with a UIDENT")
      ]) l
]
;

value extract_branches = fun [
  None -> []
| Some <:expr< fun [ $list:l$ ] >> -> l
]
;

type loc_mode_t = [ NoLoc | AutoLoc  | CustomLoc of custom_loc_t ]
and custom_loc_t = { loc_varname : lident ; loc_type : ctyp ; loc_function_name : lident }
and node_mode_t = [ Normal | Hashcons | Unique ]
and entrypoint_t = {
    entry_name : string [@name name;]
  ; grammar_entry : expr
  ; type_name : lident
}
and pertype_t = {
  custom_branches_code : option expr 
; custom_branches : (alist lident case_branch) [@computed extract_case_branches custom_branches_code;]
; add_branches_patt_code : option expr 
; add_branches_patt : (list case_branch) [@computed extract_branches add_branches_patt_code;]
; add_branches_expr_code : option expr 
; add_branches_expr : (list case_branch) [@computed extract_branches add_branches_patt_code;]
}
and custom_t = {
  pattern : expr
; expression : expr
; function_name : lident
}
and t = {
  optional : bool
; plugin_name : string
; default_data_source_module_expr : expr [@name default_data_source_module;]
; default_data_source_module_longid : longid [@computed longid_of_expr default_data_source_module_expr;]
; default_raw_quotation_source_module : option expr [@name default_quotation_source_module;]
; default_quotation_source_module_expr : expr
      [@computed (match default_raw_quotation_source_module with [
          None -> default_data_source_module_expr
        | Some x -> x
        ]);]
; default_quotation_source_module_longid : longid [@computed longid_of_expr default_quotation_source_module_expr;]

; expr_meta_module_expr : expr [@name expr_meta_module;]
; expr_meta_module_longid : longid [@computed longid_of_expr expr_meta_module_expr;]

; patt_meta_module_expr : expr [@name patt_meta_module;]
; patt_meta_module_longid : longid [@computed longid_of_expr patt_meta_module_expr;]

; external_types : (alist ctyp expr) [@default [];]
; hashconsed : bool [@default False;]
; uniqified : bool [@default False;]
; pertype : (alist lident pertype_t) [@default [];]
; custom_type : (alist ctyp custom_t) [@default [];]
; type_decls : list (string * MLast.type_decl) [@computed type_decls;]
; entrypoints : list entrypoint_t [@default [];]
; node_mode : node_mode_t [@default Normal;]
; loc_mode : loc_mode_t [@default AutoLoc;]
} [@@deriving params {
    formal_args = {
      t = [ type_decls ]
    }
  ; validators = { t = fun params ->
      if params.hashconsed && params.uniqified then
        Result.Error "at most one of hashconsed and uniqified can be true"
      else Result.Ok True }
  };]
;

value build_context loc ctxt tdl =
  let type_decls = List.map (fun (MLast.{tdNam=tdNam} as td) ->
      (tdNam |> uv |> snd |> uv, td)
    ) tdl in
  let optarg =
    let l = List.map (fun (k, e) -> (<:patt< $lid:k$ >>, e)) (Ctxt.options ctxt) in
    <:expr< { $list:l$ } >> in
  params type_decls optarg
;

value to_patt loc (v, ty) = <:patt< ($lid:v$ : $ty$) >> ;
value to_expr loc (v, ty) = <:expr< ($lid:v$ : $ty$) >> ;

value left_right_eval_list_expr loc el =
  let l = el |> List.mapi (fun i e ->
                    let v = Fmt.(str "__v_%d__" i) in
                    let varexp = <:expr< $lid:v$ >> in
                    let letb = (<:patt< $lid:v$ >>, e, <:vala< [] >>) in
                    (varexp, letb)) in
  let varexps = List.map fst l in
  let varlist = convert_up_list_expr loc varexps in
  let letbs = List.map snd l in
  <:expr< let $list:letbs$ in $varlist$ >>
;

value generate_conversion arg rc rho in_patt (name, t) =
  let custom_branches = match AList.assoc name rc.pertype with [
    x -> x.custom_branches
  | exception Not_found -> []
  ] in
  let add_branches = match (in_patt, AList.assoc name rc.pertype) with [
    (True, x) -> x.add_branches_patt
  | (False, x) -> x.add_branches_expr
  | exception Not_found -> []
  ] in
  let rec genrec = fun [
    <:ctyp< $_$ == $ty$ >> -> genrec ty
  | <:ctyp:< [ $list:branches$ ] >> ->
      let branches = List.map (fun [
          <:constructor< $uid:uid$ of $list:tyl$ >> ->
          let argvars = List.mapi (fun i ty -> (Printf.sprintf "v_%d" i,ty)) tyl in
          let argpatt = Patt.applist <:patt< $longid:rc.default_quotation_source_module_longid$ . $uid:uid$ >> (List.map (to_patt loc) argvars) in
          let argexps = List.map (fun (v, ty) -> <:expr< $genrec ty$ $lid:v$ >>) argvars in
          let arglist = left_right_eval_list_expr loc argexps in
          match List.assoc uid custom_branches with [
            x -> x
          | exception Not_found ->
          (argpatt, <:vala< None >>, <:expr< C.node_no_loc ~{prefix=data_prefix} $str:uid$ $arglist$ >>)
          ]
        ]) branches in
      <:expr< fun [ $list:add_branches@branches$ ] >>

  | <:ctyp:< { $list:ltl$ } >> ->
      let argvars = List.map (fun (_, id, _, ty, _) -> (id, ty)) ltl in
      let lpl = List.map (fun (id, _) -> (<:patt< $lid:id$ >>, <:patt< $lid:id$ >>)) argvars in
      let argpat = <:patt< { $list:lpl$ } >> in
      let members = List.map (fun (id, ty) ->
          let label = <:patt< $longid:rc.default_quotation_source_module_longid$ . $lid:id$ >> in
          <:expr< (let loc = Ploc.dummy in $Q_ast.Meta_E.patt label$, $genrec ty$ $lid:id$) >>) argvars in
      let reclist = left_right_eval_list_expr loc members in
      <:expr< fun $argpat$ -> C.record $reclist$ >>

  | <:ctyp:< ( $list:l$ ) >> ->
      let argvars = List.mapi (fun i ty -> (Printf.sprintf "v_%d" i, ty)) l in
      let argpat = <:patt< ( $list:List.map (to_patt loc) argvars$ ) >> in
      let members = List.map (fun (v,ty) -> <:expr< $genrec ty$ $lid:v$ >>) argvars in
      let tuplist = left_right_eval_list_expr loc members in
      <:expr< fun $argpat$ -> C.tuple $tuplist$ >>

  | <:ctyp:< vala $ty$ >> | <:ctyp:< Ploc.vala $ty$ >> | <:ctyp:< Pcaml.t $ty$ >> ->
    <:expr< C.vala $genrec ty$ >>

  | <:ctyp:< int >> -> <:expr< C.int >>
  | <:ctyp:< bool >> -> <:expr< C.bool >>
  | <:ctyp:< list >> -> <:expr< C.list >>
  | <:ctyp:< option >> -> <:expr< C.option >>

  | <:ctyp:< $_$ $_$ >> as ty ->
    let (ty, argtys) = Ctyp.unapplist ty in
    Expr.applist (genrec ty) (List.map genrec argtys)

  | <:ctyp:< string >> -> <:expr< C.string >>

  | <:ctyp:< $lid:lid$ >> when List.mem_assoc lid rc.type_decls -> <:expr< $lid:lid$ __ctxt__ >>
  | cty when AList.mem ~{cmp=Reloc.eq_ctyp} cty rc.custom_type ->
     let loc = loc_of_ctyp cty in
     let ct = AList.assoc ~{cmp=Reloc.eq_ctyp} cty rc.custom_type in
     <:expr< $lid:ct.function_name$ __ctxt__ >>
  | <:ctyp:< $lid:id$ >> when List.mem_assoc id rho ->
    <:expr< $lid:List.assoc id rho$ >>
  | ty -> Ploc.raise (loc_of_ctyp ty)
      (Failure Fmt.(str "generate_conversion: unhandled ctyp %a"
                      Pp_MLast.pp_ctyp ty))
  ] in
  match t with [
    <:ctyp:< Hashcons.hash_consed $t$ >> | <:ctyp:< hash_consed $t$ >> when rc.hashconsed ->
    let branch =
    if in_patt then
      let node_label = <:patt< Hashcons . node >> in
      (<:patt< x >>, <:vala< None >>, 
       <:expr< C.record [(let loc = Ploc.dummy in $Q_ast.Meta_E.patt node_label$, $genrec t$ x.Hashcons.node) ] >>)
    else 
      (<:patt< x >>, <:vala< None >>,
       <:expr< C.app_no_loc ~{prefix=data_prefix} $str:"make_"^name$ [ $genrec t$ x.Hashcons.node ] >>) in
    <:expr< fun [ $list:add_branches@[branch]$ ] >>

  | <:ctyp:< Pa_ppx_unique_runtime.Unique.unique $t$ >> | <:ctyp:< unique $t$ >> when rc.uniqified ->
    let branch =
    if in_patt then
      let node_label = <:patt< Pa_ppx_unique_runtime.Unique . node >> in
      (<:patt< x >>, <:vala< None >>, 
       <:expr< C.record [(let loc = Ploc.dummy in $Q_ast.Meta_E.patt node_label$, $genrec t$ x.Pa_ppx_unique_runtime.Unique.node) ] >>)
    else 
      (<:patt< x >>, <:vala< None >>,
       <:expr< C.app_no_loc ~{prefix=data_prefix} $str:"make_"^name$ [ $genrec t$ x.Pa_ppx_unique_runtime.Unique.node ] >>) in
    <:expr< fun [ $list:add_branches@[branch]$ ] >>

  | _ -> genrec t
  ]
;

value generate_converter arg rc in_patt (_, td) =
  let loc = loc_of_type_decl td in
  let name = td.tdNam |> uv |> snd |> uv in
  if AList.mem ~{cmp=Reloc.eq_ctyp} <:ctyp< $lid:name$ >> rc.custom_type then None else
  let rho =
    let tyvars = td.tdPrm |> uv in
    List.mapi (fun i -> fun [
        (<:vala< None >>, _) ->
        Ploc.raise loc (Failure Fmt.(str "generate_converter: %s: formal type-vars must all be named"
                                       name))
      | (<:vala< Some id >>, _) -> (id, Printf.sprintf "sub_%d" i)
      ]) tyvars in
  let body = generate_conversion arg rc rho in_patt (name, monomorphize_ctyp td.tdDef) in
  let body = match body with [
    <:expr< fun [ $list:_$ ] >> -> body
  | _ -> <:expr< fun x -> $body$ x >>
  ] in
  let body = match td.tdDef with [
    <:ctyp< $_$ $_$ >> -> <:expr< fun x -> $body$ x >>
  | _ -> body
  ] in
  let fbody = List.fold_right (fun (id, fname) rhs -> <:expr< fun ( $lid:fname$ : $lid:id$ -> C.t) -> $rhs$ >>) rho body in
  let fbody = <:expr< fun __ctxt__ -> $fbody$ >> in
  let fbody = List.fold_right (fun (id, _) rhs -> <:expr< fun (type $lid:id$) -> $rhs$ >>) rho fbody in
  let ftype =
    if rho = [] then
      <:ctyp< ctxt_t -> $longid:rc.default_quotation_source_module_longid$ . $lid:name$ -> C.t >>
    else
      let thety = Ctyp.applist <:ctyp< $lid:name$ >> (List.map (fun (id, _) -> <:ctyp< ' $id$ >>) rho) in
      let rhsty = List.fold_right (fun (id, _) rhs -> <:ctyp< ( ' $id$ -> C.t) -> $rhs$ >>) rho <:ctyp< $thety$ -> C.t >> in
      <:ctyp< ! $list:List.map fst rho$ . ctxt_t -> $rhsty$ >> in
  Some (<:patt< ( $lid:name$ : $ftype$ ) >>, fbody, <:vala< [] >>)
;

value generate_custom_expr_converter arg rc (_, custom) =
  let loc = loc_of_expr custom.expression in
  let fbody = <:expr< let loc = Ploc.dummy in $custom.expression$ >> in
  (<:patt< $lid:custom.function_name$ >>, fbody, <:vala< [] >>)
;

value generate_custom_patt_converter arg rc (_, custom) =
  let loc = loc_of_expr custom.pattern in
  let fbody = <:expr< let loc = Ploc.dummy in $custom.pattern$ >> in
  (<:patt< $lid:custom.function_name$ >>, fbody, <:vala< [] >>)
;

value generate_meta_e_bindings loc arg rc in_patt tdl =
  let l = List.filter_map (generate_converter arg rc in_patt) tdl in
  let customl = List.map (generate_custom_expr_converter arg rc) rc.custom_type in
  let data_prefix = Q_ast.Meta_E.longid rc.default_data_source_module_longid in
  ([(<:patt< data_prefix >>, <:expr< let loc = Ploc.dummy in $data_prefix$ >>, <:vala< [] >>)], l@customl)
;

value generate_meta_p_bindings loc arg rc in_patt tdl =
  let l = List.filter_map (generate_converter arg rc in_patt) tdl in
  let customl = List.map (generate_custom_patt_converter arg rc) rc.custom_type in
  let data_prefix = Q_ast.Meta_E.longid rc.default_data_source_module_longid in
  ([(<:patt< data_prefix >>, <:expr< let loc = Ploc.dummy in $data_prefix$ >>, <:vala< [] >>)], l@customl)
;

value generate_entrypoint loc arg rc (ep : entrypoint_t) =
  let apply_fun = match (rc.node_mode, rc.loc_mode) with [
        (Normal, AutoLoc) -> <:expr< Pa_ppx_q_ast_runtime.apply_entry >>
      | (Hashcons, AutoLoc) -> <:expr< Pa_ppx_q_ast_runtime.hc_apply_entry >>
      | (Unique, AutoLoc) -> <:expr< Pa_ppx_q_ast_runtime.unique_apply_entry >>
      | (Normal, NoLoc) ->   <:expr< Pa_ppx_q_ast_runtime.noloc_apply_entry >>
      | (Normal, CustomLoc _) ->   <:expr< Pa_ppx_q_ast_runtime.customloc_apply_entry >>
      | (Hashcons, CustomLoc _) ->   <:expr< Pa_ppx_q_ast_runtime.customloc_hc_apply_entry >>
      | (Unique, CustomLoc _) ->   <:expr< Pa_ppx_q_ast_runtime.customloc_unique_apply_entry >>
      ] in
  <:str_item< Quotation.add $str:ep.entry_name$
  ($apply_fun$ $ep.grammar_entry$ E . $lid:ep.type_name$ P . $lid:ep.type_name$) >>
;

value generate_ctxt_ctyps loc arg rc =
  match (rc.node_mode, rc.loc_mode) with [
      (Normal, AutoLoc) -> (<:ctyp< unit >>,<:ctyp< unit >>)
    | (Hashcons, AutoLoc) -> (<:ctyp< unit >>,<:ctyp< unit >>)
    | (Unique, AutoLoc) -> (<:ctyp< unit >>,<:ctyp< unit >>)
    | (Normal, NoLoc) -> (<:ctyp< unit >>,<:ctyp< unit >>)
    | (_, CustomLoc _) -> (<:ctyp< Pa_ppx_q_ast_runtime.Locate.t >>,<:ctyp< unit >>)
    ]
;

end
;

value str_item_gen_q_ast name arg = fun [
  <:str_item:< type $_flag:_$ $list:tdl$ >> ->
    let rc = QAST.build_context loc arg tdl in
    let rc = match rc.loc_mode with [
          CustomLoc l ->
          let expression =
            let loc_expr_expr = expr_as_expr loc l.loc_varname in
            <:expr< fun ctxt _ -> $loc_expr_expr$ >> in
          let pattern =
            let loc_patt_expr = patt_as_expr loc l.loc_varname in
            let any_patt_expr = patt_as_expr loc "_" in
            <:expr< fun ctxt _ -> if not (Pa_ppx_q_ast_runtime.Locate.locate ctxt) || Pa_ppx_q_ast_runtime.Locate.get ctxt > 0 then $any_patt_expr$ else $loc_patt_expr$ >> in
          let c = QAST.{ pattern = pattern ; expression = expression ; function_name = l.loc_function_name } in
          { (rc) with custom_type = [(l.QAST.loc_type, c) :: rc.custom_type] }
        | _ -> rc
        ] in
    let (meta_e_data_prefix_bindings, meta_e_bindings) = QAST.generate_meta_e_bindings loc arg rc False rc.QAST.type_decls in
    let (meta_p_data_prefix_bindings, meta_p_bindings) = QAST.generate_meta_p_bindings loc arg rc True rc.QAST.type_decls in
    let (patt_ctxt_ctyp, expr_ctxt_ctyp) = QAST.generate_ctxt_ctyps loc arg rc in
    let ep_sil = List.map (QAST.generate_entrypoint loc arg rc) rc.entrypoints in
    <:str_item< declare module E = struct
                module C = $module_expr_of_longident rc.expr_meta_module_longid$ ;
                value $list:meta_e_data_prefix_bindings$ ;
                type ctxt_t = $expr_ctxt_ctyp$ ;
                value rec $list:meta_e_bindings$ ;
                end ;
                module P = struct
                module C = $module_expr_of_longident rc.patt_meta_module_longid$ ;
                value $list:meta_p_data_prefix_bindings$ ;
                type ctxt_t = $patt_ctxt_ctyp$ ;
                value rec $list:meta_p_bindings$ ;
                end ;
                declare $list:ep_sil$ end ;
                end >>
| _ -> assert False ]
;

Pa_deriving.(Registry.add PI.{
  name = "q_ast"
; alternates = []
; options = [
    "optional"
  ; "default_data_source_module"
  ; "default_quotation_source_module"
  ; "expr_meta_module"
  ; "patt_meta_module"
  ; "hashconsed"
  ; "uniqified"
  ; "pertype"
  ; "custom_type"
  ; "entrypoints"
  ; "node_mode"
  ; "loc_mode"
  ]
; default_options = let loc = Ploc.dummy in [
    ("optional", <:expr< False >>)
  ; ("expr_meta_module", <:expr< Pa_ppx_q_ast_runtime.MetaE >>)
  ; ("patt_meta_module", <:expr< Pa_ppx_q_ast_runtime.MetaP >>)
  ; ("external_types", <:expr< () >>)
  ; ("pertype", <:expr< () >>)
  ; ("custom_type", <:expr< () >>)
  ; ("node_mode", <:expr< Normal >>)
  ; ("loc_mode", <:expr< AutoLoc >>)
  ]
; alg_attributes = []
; expr_extensions = []
; ctyp_extensions = []
; expr = (fun arg e -> assert False)
; ctyp = (fun arg e -> assert False)
; str_item = str_item_gen_q_ast
; sig_item = (fun arg e -> assert False)
})
;

