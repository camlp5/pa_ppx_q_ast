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

type pertype_t = {
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
}
and t = {
  optional : bool
; plugin_name : string
; data_source_module_expr : expr [@name data_source_module;]
; data_source_module_longid : longid [@computed longid_of_expr data_source_module_expr;]
; raw_quotation_source_module : option expr [@name quotation_source_module;]
; quotation_source_module_expr : expr
      [@computed (match raw_quotation_source_module with [
          None -> data_source_module_expr
        | Some x -> x
        ]);]
; quotation_source_module_longid : longid [@computed longid_of_expr quotation_source_module_expr;]

; expr_meta_module_expr : expr [@name expr_meta_module;]
; expr_meta_module_longid : longid [@computed longid_of_expr expr_meta_module_expr;]

; patt_meta_module_expr : expr [@name patt_meta_module;]
; patt_meta_module_longid : longid [@computed longid_of_expr patt_meta_module_expr;]

; external_types : (alist ctyp expr) [@default [];]
; hashconsed : bool [@default False;]
; uniqified : bool [@default False;]
; pertype : (alist lident pertype_t) [@default [];]
; custom_type : (alist lident custom_t) [@default [];]
; type_decls : list (string * MLast.type_decl) [@computed type_decls;]
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
          let argpatt = Patt.applist <:patt< $uid:uid$ >> (List.map (to_patt loc) argvars) in
          let arglist = List.fold_right (fun (v, ty) rhs ->
              <:expr< [ $genrec ty$ $lid:v$ :: $rhs$ ] >>) argvars <:expr< [] >> in
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
          let label = <:patt< $longid:rc.quotation_source_module_longid$ . $lid:id$ >> in
          <:expr< (let loc = Ploc.dummy in $Q_ast.Meta_E.patt label$, $genrec ty$ $lid:id$) >>) argvars in
      let reclist = List.fold_right (fun e rhs -> <:expr< [ $e$ :: $rhs$ ] >>) members <:expr< [] >> in
      <:expr< fun $argpat$ -> C.record $reclist$ >>

  | <:ctyp:< ( $list:l$ ) >> ->
      let argvars = List.mapi (fun i ty -> (Printf.sprintf "v_%d" i, ty)) l in
      let argpat = <:patt< ( $list:List.map (to_patt loc) argvars$ ) >> in
      let members = List.map (fun (v,ty) -> <:expr< $genrec ty$ $lid:v$ >>) argvars in
      let tuplist = List.fold_right (fun e rhs -> <:expr< [ $e$ :: $rhs$ ] >>) members <:expr< [] >> in
      <:expr< fun $argpat$ -> C.tuple $tuplist$ >>

  | <:ctyp:< vala $ty$ >> | <:ctyp:< Ploc.vala $ty$ >> | <:ctyp:< Pcaml.t $ty$ >> ->
    <:expr< C.vala $genrec ty$ >>

  | <:ctyp:< int >> -> <:expr< C.int >>
  | <:ctyp:< bool >> -> <:expr< C.bool >>
  | <:ctyp:< list >> -> <:expr< C.list >>
  | <:ctyp:< option >> -> <:expr< C.option >>
  | <:ctyp:< Ploc.t >> | <:ctyp:< loc >> -> <:expr< fun _ -> C.loc_v () >>

  | <:ctyp:< $_$ $_$ >> as ty ->
    let (ty, argtys) = Ctyp.unapplist ty in
    Expr.applist (genrec ty) (List.map genrec argtys)

  | <:ctyp:< string >> -> <:expr< C.string >>

  | <:ctyp:< $lid:lid$ >> when List.mem_assoc lid rc.type_decls -> <:expr< $lid:lid$ >>
  | <:ctyp:< $lid:lid$ >> when List.mem_assoc lid rc.custom_type -> <:expr< $lid:lid$ >>
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
  let fbody = List.fold_right (fun (id, _) rhs -> <:expr< fun (type $lid:id$) -> $rhs$ >>) rho fbody in
  let ftype =
    if rho = [] then
      <:ctyp< $longid:rc.quotation_source_module_longid$ . $lid:name$ -> C.t >>
    else
      let thety = Ctyp.applist <:ctyp< $lid:name$ >> (List.map (fun (id, _) -> <:ctyp< ' $id$ >>) rho) in
      let rhsty = List.fold_right (fun (id, _) rhs -> <:ctyp< ( ' $id$ -> C.t) -> $rhs$ >>) rho <:ctyp< $thety$ -> C.t >> in
      <:ctyp< ! $list:List.map fst rho$ . $rhsty$ >> in
  (<:patt< ( $lid:name$ : $ftype$ ) >>, fbody, <:vala< [] >>)
;

value generate_custom_expr_converter arg rc (id, custom) =
  let loc = loc_of_expr custom.expression in
  let fbody = <:expr< let loc = Ploc.dummy in $custom.expression$ >> in
  (<:patt< $lid:id$ >>, fbody, <:vala< [] >>)
;

value generate_custom_patt_converter arg rc (id, custom) =
  let loc = loc_of_expr custom.pattern in
  let fbody = <:expr< let loc = Ploc.dummy in $custom.pattern$ >> in
  (<:patt< $lid:id$ >>, fbody, <:vala< [] >>)
;

value generate_meta_e_bindings loc arg rc in_patt tdl =
  let l = List.map (generate_converter arg rc in_patt) tdl in
  let customl = List.map (generate_custom_expr_converter arg rc) rc.custom_type in
  let data_prefix = Q_ast.Meta_E.longid rc.data_source_module_longid in
  ([(<:patt< data_prefix >>, <:expr< let loc = Ploc.dummy in $data_prefix$ >>, <:vala< [] >>)], l@customl)
;

value generate_meta_p_bindings loc arg rc in_patt tdl =
  let l = List.map (generate_converter arg rc in_patt) tdl in
  let customl = List.map (generate_custom_patt_converter arg rc) rc.custom_type in
  let data_prefix = Q_ast.Meta_E.longid rc.data_source_module_longid in
  ([(<:patt< data_prefix >>, <:expr< let loc = Ploc.dummy in $data_prefix$ >>, <:vala< [] >>)], l@customl)
;

end
;

value str_item_gen_q_ast name arg = fun [
  <:str_item:< type $_flag:_$ $list:tdl$ >> ->
    let rc = QAST.build_context loc arg tdl in
    let (meta_e_data_prefix_bindings, meta_e_bindings) = QAST.generate_meta_e_bindings loc arg rc False rc.QAST.type_decls in
    let (meta_p_data_prefix_bindings, meta_p_bindings) = QAST.generate_meta_p_bindings loc arg rc True rc.QAST.type_decls in
    <:str_item< declare module E = struct
                module C = $module_expr_of_longident rc.expr_meta_module_longid$ ;
                value $list:meta_e_data_prefix_bindings$ ;
                value rec $list:meta_e_bindings$ ;
                end ;
                module P = struct
                module C = $module_expr_of_longident rc.patt_meta_module_longid$ ;
                value $list:meta_p_data_prefix_bindings$ ;
                value rec $list:meta_p_bindings$ ;
                end ;
                end >>
| _ -> assert False ]
;

Pa_deriving.(Registry.add PI.{
  name = "q_ast"
; alternates = []
; options = [
    "optional"
  ; "data_source_module"
  ; "quotation_source_module"
  ; "expr_meta_module"
  ; "patt_meta_module"
  ; "hashconsed"
  ; "uniqified"
  ; "pertype"
  ; "custom_type"
  ]
; default_options = let loc = Ploc.dummy in [
    ("optional", <:expr< False >>)
  ; ("expr_meta_module", <:expr< Pa_ppx_q_ast_runtime.MetaE >>)
  ; ("patt_meta_module", <:expr< Pa_ppx_q_ast_runtime.MetaP >>)
  ; ("external_types", <:expr< () >>)
  ; ("pertype", <:expr< () >>)
  ; ("custom_type", <:expr< () >>)
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

