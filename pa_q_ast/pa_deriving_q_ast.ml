(* camlp5r *)
(* pa_deriving_q_ast.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

#load "pa_extend.cmo";
#load "q_MLast.cmo";
#load "pa_macro.cmo";
#load "pa_macro_gram.cmo";
#load "pa_extfun.cmo";

open Asttools;
open MLast;
open Pa_ppx_base ;
open Pa_passthru ;
open Ppxutil ;
open Surveil ;
open Pa_deriving_base ;
open Pa_ppx_utils ;

value debug = Pa_passthru.debug ;

value canon_ctyp ty = Reloc.ctyp (fun _ -> Ploc.dummy) 0 ty ;

value string_list_of_expr e =
  let rec lrec = fun [
    <:expr< $uid:uid$ >> -> [uid]
  | <:expr< $e1$ . $e2$ >> -> (lrec e1)@(lrec e2)
  | e -> Ploc.raise (loc_of_expr e) (Failure "string_list_of_expr: unexpected expr")
  ] in
  lrec e
;
value longid_of_expr e =
  let l = string_list_of_expr e in
  Asttools.longident_of_string_list (loc_of_expr e) l
;

module QAST = struct
type t = {
  data_source_module_expr : MLast.expr
; data_source_module_longid : MLast.longid
; quotation_source_module_expr : MLast.expr
; quotation_source_module_longid : MLast.longid
; expr_meta_module_longid : MLast.longid
; patt_meta_module_longid : MLast.longid
; type_decls : list (string * MLast.type_decl)
; external_types : list (MLast.ctyp * MLast.expr)
; hashconsed : bool
}
;

value build_context loc ctxt tdl =
  let type_decls = List.map (fun (MLast.{tdNam=tdNam} as td) ->
      (tdNam |> uv |> snd |> uv, td)
    ) tdl in
  let open Ctxt in
  let data_source_module_expr = match option ctxt "data_source_module" with [
    x -> x
  | exception Failure _ ->
  Ploc.raise loc (Failure "pa_deriving_q_ast: option data_source_module must be specified")
  ] in
  let data_source_module_longid = longid_of_expr data_source_module_expr in
  let quotation_source_module_expr = match option ctxt "quotation_source_module" with [
    <:expr< () >> -> data_source_module_expr
  | x -> x
  | exception Failure _ ->
  Ploc.raise loc (Failure "pa_deriving_q_ast: option quotation_source_module must be specified")
  ] in
  let quotation_source_module_longid = longid_of_expr quotation_source_module_expr in
  let expr_meta_module_expr = match option ctxt "expr_meta_module" with [
    x -> x
  | exception Failure _ ->
  Ploc.raise loc (Failure "pa_deriving_q_ast: option expr_meta_module must be specified")
  ] in
  let expr_meta_module_longid = longid_of_expr expr_meta_module_expr in
  let patt_meta_module_expr = match option ctxt "patt_meta_module" with [
    x -> x
  | exception Failure _ ->
  Ploc.raise loc (Failure "pa_deriving_q_ast: option patt_meta_module must be specified")
  ] in
  let patt_meta_module_longid = longid_of_expr patt_meta_module_expr in
  let external_types = match option ctxt "external_types" with [
      <:expr:< { $list:lel$ } >> ->
        List.map (fun (p, e) ->
            let ty = match p with [
              <:patt:< $lid:lid$ >> -> canon_ctyp <:ctyp< $lid:lid$ >>
            | <:patt:< $longid:li$ . $lid:lid$ >> -> canon_ctyp <:ctyp< $longid:li$ . $lid:lid$ >>
            | p -> Ploc.raise (loc_of_patt p)
                (Failure Fmt.(str "pa_deriving_hashcons: key in external_types not a type:@ %a"
                                Pp_MLast.pp_patt p))
            ] in (ty, e)
          ) lel
    | <:expr< () >> -> []
    | _ -> Ploc.raise loc (Failure "pa_deriving_hashcons: malformed option external_types")
  ] in
  let hashconsed = match option ctxt "hashconsed" with [
    <:expr< True >> -> True
  | <:expr< False >> -> False
  | _ -> Ploc.raise loc (Failure "pa_deriving_hashcons: malformed option hashconsed")
  ] in
  {
    data_source_module_expr = data_source_module_expr
  ; data_source_module_longid = data_source_module_longid
  ; quotation_source_module_expr = quotation_source_module_expr
  ; quotation_source_module_longid = quotation_source_module_longid
  ; expr_meta_module_longid = expr_meta_module_longid
  ; patt_meta_module_longid = patt_meta_module_longid
  ; type_decls = type_decls
  ; external_types = external_types
  ; hashconsed = hashconsed
  }
;

value to_patt loc (v, ty) = <:patt< ($lid:v$ : $ty$) >> ;
value to_expr loc (v, ty) = <:expr< ($lid:v$ : $ty$) >> ;

value generate_conversion arg rc rho in_patt (name, t) =
  let rec genrec = fun [
    <:ctyp< $_$ == $ty$ >> -> genrec ty
  | <:ctyp:< [ $list:branches$ ] >> ->
      let branches = List.map (fun [
          <:constructor< $uid:uid$ of $list:tyl$ >> ->
          let argvars = List.mapi (fun i ty -> (Printf.sprintf "v_%d" i,ty)) tyl in
          let argpatt = Patt.applist <:patt< $uid:uid$ >> (List.map (to_patt loc) argvars) in
          let arglist = List.fold_right (fun (v, ty) rhs ->
              <:expr< [ $genrec ty$ $lid:v$ :: $rhs$ ] >>) argvars <:expr< [] >> in
          (argpatt, <:vala< None >>, <:expr< C.node_no_loc ~{prefix=data_prefix} $str:uid$ $arglist$ >>)
        ]) branches in
      <:expr< fun [ $list:branches$ ] >>

  | <:ctyp:< { $list:ltl$ } >> ->
      let argvars = List.map (fun (_, id, _, ty, _) -> (id, ty)) ltl in
      let lpl = List.map (fun (id, _) -> (<:patt< $lid:id$ >>, <:patt< $lid:id$ >>)) argvars in
      let argpat = <:patt< { $list:lpl$ } >> in
      let members = List.map (fun (id, ty) ->
          let label = <:patt< $longid:rc.data_source_module_longid$ . $lid:id$ >> in
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

  | <:ctyp:< bool >> -> <:expr< C.bool >>
  | <:ctyp:< list >> -> <:expr< C.list >>
  | <:ctyp:< option >> -> <:expr< C.option >>
  | <:ctyp:< Ploc.t >> | <:ctyp:< loc >> -> <:expr< fun _ -> C.loc_v () >>

  | <:ctyp:< $_$ $_$ >> as ty ->
    let (ty, argtys) = Ctyp.unapplist ty in
    Expr.applist (genrec ty) (List.map genrec argtys)

  | <:ctyp:< string >> -> <:expr< C.string >>

  | <:ctyp:< $lid:lid$ >> when List.mem_assoc lid rc.type_decls -> <:expr< $lid:lid$ >>
  | <:ctyp:< $lid:id$ >> when List.mem_assoc id rho ->
    <:expr< $lid:List.assoc id rho$ >>
  | ty -> Ploc.raise (loc_of_ctyp ty)
      (Failure Fmt.(str "generate_conversion: unhandled ctyp %a"
                      Pp_MLast.pp_ctyp ty))
  ] in
  match t with [
    <:ctyp:< Hashcons.hash_consed $t$ >> | <:ctyp:< hash_consed $t$ >> when rc.hashconsed ->
    if in_patt then
      let node_label = <:patt< Hashcons . node >> in
      <:expr< fun x -> C.record [(let loc = Ploc.dummy in $Q_ast.Meta_E.patt node_label$, $genrec t$ x.Hashcons.node) ] >>
    else 
      <:expr< fun x -> C.app_no_loc ~{prefix=data_prefix} $str:name$ [ $genrec t$ x.Hashcons.node ] >>

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
      <:ctyp< $longid:rc.data_source_module_longid$ . $lid:name$ -> C.t >>
    else
      let thety = Ctyp.applist <:ctyp< $lid:name$ >> (List.map (fun (id, _) -> <:ctyp< ' $id$ >>) rho) in
      let rhsty = List.fold_right (fun (id, _) rhs -> <:ctyp< ( ' $id$ -> C.t) -> $rhs$ >>) rho <:ctyp< $thety$ -> C.t >> in
      <:ctyp< ! $list:List.map fst rho$ . $rhsty$ >> in
  (<:patt< ( $lid:name$ : $ftype$ ) >>, fbody, <:vala< [] >>)
;

value generate_meta_e_bindings loc arg rc in_patt tdl =
  let l = List.map (generate_converter arg rc in_patt) tdl in
  let data_prefix = Q_ast.Meta_E.expr rc.data_source_module_expr in
  ([(<:patt< data_prefix >>, <:expr< let loc = Ploc.dummy in $data_prefix$ >>, <:vala< [] >>)], l)
;

value generate_meta_p_bindings loc arg rc in_patt tdl =
  let l = List.map (generate_converter arg rc in_patt) tdl in
  let data_prefix = Q_ast.Meta_E.longid rc.data_source_module_longid in
  ([(<:patt< data_prefix >>, <:expr< let loc = Ploc.dummy in $data_prefix$ >>, <:vala< [] >>)], l)
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
; options = ["optional"; "data_source_module"; "quotation_source_module"; "expr_meta_module"; "patt_meta_module"; "hashconsed"]
; default_options = let loc = Ploc.dummy in [
    ("optional", <:expr< False >>)
  ; ("quotation_source_module", <:expr< () >>)
  ; ("expr_meta_module", <:expr< Q_ast_base.E_MetaSig >>)
  ; ("patt_meta_module", <:expr< Q_ast_base.P_MetaSig >>)
  ; ("external_types", <:expr< () >>)
  ; ("hashconsed", <:expr< False >>)
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

