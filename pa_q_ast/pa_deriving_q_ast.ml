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
  source_module_expr : MLast.expr
; source_module_longid : MLast.longid
; type_decls : list (string * MLast.type_decl)
}
;

value build_context loc ctxt tdl =
  let type_decls = List.map (fun (MLast.{tdNam=tdNam} as td) ->
      (tdNam |> uv |> snd |> uv, td)
    ) tdl in
  let open Ctxt in
  let source_module_expr = match option ctxt "source_module" with [
    x -> x
  | exception Failure _ ->
  Ploc.raise loc (Failure "pa_deriving_q_ast: option source_module must be specified")
  ] in
  let source_module_longid = longid_of_expr source_module_expr in
  {
    source_module_expr = source_module_expr
  ; source_module_longid = source_module_longid
  ; type_decls = type_decls
  }
;

value to_patt loc (v, ty) = <:patt< ($lid:v$ : $ty$) >> ;
value to_expr loc (v, ty) = <:expr< ($lid:v$ : $ty$) >> ;

value generate_conversion arg rc rho t =
  let rec genrec = fun [
    <:ctyp< $_$ == $ty$ >> -> genrec ty
  | <:ctyp:< [ $list:branches$ ] >> ->
      let branches = List.map (fun [
          <:constructor< $uid:uid$ of $list:tyl$ >> ->
          let argvars = List.mapi (fun i ty -> (Printf.sprintf "v_%d" i,ty)) tyl in
          let argpatt = Patt.applist <:patt< $uid:uid$ >> (List.map (to_patt loc) argvars) in
          let arglist = List.fold_right (fun (v, ty) rhs ->
              <:expr< [ $genrec ty$ $lid:v$ :: $rhs$ ] >>) argvars <:expr< [] >> in
          (argpatt, <:vala< None >>, <:expr< C.node_no_loc ~{prefix=prefix} $str:uid$ $arglist$ >>)
        ]) branches in
      <:expr< fun [ $list:branches$ ] >>

    | <:ctyp:< vala $ty$ >> | <:ctyp:< Ploc.vala $ty$ >> | <:ctyp:< Pcaml.t $ty$ >> ->
      <:expr< C.vala $genrec ty$ >>

    | <:ctyp:< $_$ $_$ >> as ty ->
      let (ty, argtys) = Ctyp.unapplist ty in
      Expr.applist (genrec ty) (List.map genrec argtys)

    | <:ctyp:< string >> -> <:expr< C.string >>

    | <:ctyp:< $lid:lid$ >> when List.mem_assoc lid rc.type_decls -> <:expr< $lid:lid$ >>
    | <:ctyp:< ' $id$ >> when List.mem_assoc id rho ->
      <:expr< $lid:List.assoc id rho$ >>
    | ty -> Ploc.raise (loc_of_ctyp ty)
      (Failure Fmt.(str "generate_conversion: unhandled ctyp %a"
                      Pp_MLast.pp_ctyp ty))
  ] in
  genrec t
;

value generate_meta_e_binding arg rc (_, td) =
  let loc = loc_of_type_decl td in
  let name = td.tdNam |> uv |> snd |> uv in
  let rho =
    let tyvars = td.tdPrm |> uv in
    List.mapi (fun i -> fun [
        (<:vala< None >>, _) ->
        Ploc.raise loc (Failure Fmt.(str "generate_meta_e_binding: %s: formal type-vars must all be named"
                                       name))
      | (<:vala< Some id >>, _) -> (id, Printf.sprintf "sub_%d" i)
      ]) tyvars in
  let body = generate_conversion arg rc rho td.tdDef in
  let fbody = List.fold_right (fun (_, fname) rhs -> <:expr< fun $lid:fname$ -> $rhs$ >>) rho body in
  let prefix = Q_ast.Meta_E.expr rc.source_module_expr in
  (<:patt< $lid:name$ >>,
   <:expr< let prefix = let loc = Ploc.dummy in $prefix$ in
           let open Q_ast.Meta_E in
           $fbody$ >>,
   <:vala< [] >>)
;

value generate_meta_p_binding arg rc (_, td) =
  let loc = loc_of_type_decl td in
  let name = td.tdNam |> uv |> snd |> uv in
  let rho =
    let tyvars = td.tdPrm |> uv in
    List.mapi (fun i -> fun [
        (<:vala< None >>, _) ->
        Ploc.raise loc (Failure Fmt.(str "generate_meta_p_binding: %s: formal type-vars must all be named"
                                       name))
      | (<:vala< Some id >>, _) -> (id, Printf.sprintf "sub_%d" i)
      ]) tyvars in
  let body = generate_conversion arg rc rho td.tdDef in
  let fbody = List.fold_right (fun (_, fname) rhs -> <:expr< fun $lid:fname$ -> $rhs$ >>) rho body in
  let prefix = Q_ast.Meta_E.longid rc.source_module_longid in
  (<:patt< $lid:name$ >>,
   <:expr< let prefix = let loc = Ploc.dummy in $prefix$ in
           let open Q_ast.Meta_P in
           $fbody$ >>,
   <:vala< [] >>)
;

end
;

value str_item_gen_q_ast name arg = fun [
  <:str_item:< type $_flag:_$ $list:tdl$ >> ->
    let rc = QAST.build_context loc arg tdl in
    let meta_e_bindings = List.map (QAST.generate_meta_e_binding arg rc) rc.QAST.type_decls in
    let meta_p_bindings = List.map (QAST.generate_meta_p_binding arg rc) rc.QAST.type_decls in
    <:str_item< declare module E = struct
                value rec $list:meta_e_bindings$ ;
                end ;
                module P = struct
                value rec $list:meta_p_bindings$ ;
                end ;
                end >>
| _ -> assert False ]
;

Pa_deriving.(Registry.add PI.{
  name = "q_ast"
; alternates = []
; options = ["optional"; "source_module"]
; default_options = let loc = Ploc.dummy in [ ("optional", <:expr< False >>) ]
; alg_attributes = []
; expr_extensions = []
; ctyp_extensions = []
; expr = (fun arg e -> assert False)
; ctyp = (fun arg e -> assert False)
; str_item = str_item_gen_q_ast
; sig_item = (fun arg e -> assert False)
})
;

