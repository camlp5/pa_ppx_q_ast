(**pp -syntax camlp5o -package pa_ppx.import,pa_ppx_migrate,camlp5.quotations,camlp5.extfun *)
(* camlp5o *)
(* pa_string.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Pa_ppx_base
open Pa_ppx_utils
open Pa_passthru
open Ppxutil

exception Migration_error of string

let migration_error feature =
  raise (Migration_error feature)

let _migrate_list subrw0 __dt__ l =
  List.map (subrw0 __dt__) l

[%%typedecls
  [%%import: MLast.expr
    [@add [%%import: MLast.loc]]
    [@add [%%import: MLast.type_var]]
    [@with Ploc.vala := vala]
  ]
  [%%import: 'a Ploc.vala]
]
[@@deriving migrate
    { dispatch_type = dispatch_table_t
    ; dispatch_table_constructor = make_dt
    ; default_dispatchers = [
        {
          srcmod = MLast
        ; dstmod = Ploc
        ; types = [
            vala
          ]
        }
      ; {
          srcmod = MLast
        ; dstmod = MLast
        ; types = [
            class_infos
          ; longid
          ; ctyp
          ; poly_variant
          ; patt
          ; expr
          ; case_branch
          ; module_type
          ; functor_parameter
          ; sig_item
          ; with_constr
          ; module_expr
          ; str_item
          ; type_decl
          ; generic_constructor
          ; extension_constructor
          ; type_extension
          ; class_type
          ; class_sig_item
          ; class_expr
          ; class_str_item
          ; longid_lident
          ; payload
          ; attribute_body
          ; attribute
          ; attributes_no_anti
          ; attributes
          ; type_var
          ]
        }
      ]
    ; dispatchers = {
        migrate_list = {
          srctype = [%typ: 'a list]
        ; dsttype = [%typ: 'b list]
        ; code = _migrate_list
        ; subs = [ ([%typ: 'a], [%typ: 'b]) ]
        }
      ; migrate_option = {
          srctype = [%typ: 'a option]
        ; dsttype = [%typ: 'b option]
        ; subs = [ ([%typ: 'a], [%typ: 'b]) ]
        ; code = (fun subrw __dt__ x -> Option.map (subrw __dt__) x)
        }
      ; migrate_loc = {
          srctype = [%typ: loc]
        ; dsttype = [%typ: MLast.loc]
        ; code = fun __dt__ x -> x
        }
      }
    }
]

let is_constant_expr = function
    <:expr< $lid:_$ >> -> true
  | <:expr< $uid:_$ >> -> true
  | <:expr< fun [ $list:__$ ] >> -> true
  | _ -> false

let to_binding_newe i e =
  if is_constant_expr e then
    (None, e)
  else
    let loc = MLast.loc_of_expr e in
    let v = Printf.sprintf "__v%02d__" i in
    (Some (<:patt< $lid:v$ >>, e, <:vala< [] >>),
     <:expr< $lid:v$ >>)

let lreval e =
  let dt = make_dt() in
  let migrate_attribute dt a = a in
  let old_migrate_expr = dt.migrate_expr in
  let migrate_expr dt = function
      <:expr:< $uid:cid$ {$list:pel$} >> ->
       let bindings_newpel =
         pel
         |> List.mapi (fun i (p,e) ->
                let (bopt, newe) = to_binding_newe i (dt.migrate_expr dt e) in
                (bopt, (p,e))) in
       let bindings = List.filter_map fst bindings_newpel in
       let newpel = List.map snd bindings_newpel in
       let body = <:expr< $uid:cid$ { $list:newpel$ } >> in
       if bindings = [] then body else
       <:expr< let $list:bindings$ in $body$ >>

    | <:expr:< {$list:pel$} >> ->
       let bindings_newpel =
         pel
         |> List.mapi (fun i (p,e) ->
                let (bopt, newe) = to_binding_newe i (dt.migrate_expr dt e) in
                (bopt, (p,e))) in
       let bindings = List.filter_map fst bindings_newpel in
       let newpel = List.map snd bindings_newpel in
       let body = <:expr< { $list:newpel$ } >> in
       if bindings = [] then body else
       <:expr< let $list:bindings$ in $body$ >>

    | <:expr:< ( $list:el$ ) >> ->
       let bindings_newel =
         el
         |> List.mapi (fun i e ->
                match e with
                  <:expr:< ~{$lid:lab$ = $e$} >> ->
                  let (bopt, newe) = to_binding_newe i (dt.migrate_expr dt e) in
                  (bopt, <:expr< ~{$lid:lab$ = $newe$} >>)
                | _ ->
                   to_binding_newe i (dt.migrate_expr dt e)
              ) in
       let bindings = List.filter_map fst bindings_newel in
       let newel = List.map snd bindings_newel in
       let body = <:expr< ( $list:newel$ ) >> in
       if bindings = [] then body else
       <:expr< let $list:bindings$ in $body$ >>

    | <:expr:< $_$ $_$ >> as e ->
       let (f, args) = Expr.unapplist e in

       let bindings_newargs =
         args
         |> List.map (dt.migrate_expr dt)
         |> List.mapi to_binding_newe in

       let bindings = List.filter_map fst bindings_newargs in
       let newargs = List.map snd bindings_newargs in
       let body = Expr.applist f newargs in
       if bindings = [] then body else
       <:expr< let $list:bindings$ in $body$ >>


    | e ->  old_migrate_expr dt e
  in
  let dt = { (dt) with migrate_expr = migrate_expr; migrate_attribute = migrate_attribute } in
  dt.migrate_expr dt e

let install () = 
let ef = EF.mk () in 
let ef = EF.{ (ef) with
            expr = extfun ef.expr with [
    z ->
    fun arg fallback ->
      Some (lreval z)
              ]
 } in
let ef = EF.{ (ef) with
            attribute_body = extfun ef.attribute_body with [
    z ->
    fun arg fallback ->
      Some z
              ]
 } in
  Pa_passthru.(install { name = "pa_lefteval"; ef =  ef ; pass = None ; before = [] ; after = ["pa_deriving"] })
;;

install();;
