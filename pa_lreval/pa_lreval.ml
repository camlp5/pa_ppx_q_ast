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

let lreval e =
  let dt = make_dt() in
  let old_migrate_expr = dt.migrate_expr in
  let migrate_expr dt = function
      <:expr:< $uid:cid$ {$list:l$} >> ->
      let l = l |> List.map (fun (p, e) -> (p, dt.migrate_expr dt e)) in
      <:expr:< $uid:cid$ {$list:l$} >>

    | <:expr:< $_$ $_$ >> as e ->
       let (f, args) = Expr.unapplist e in
       let vars_args =
         List.mapi (fun i e ->
             let v = Printf.sprintf "__v%02d__" i in
             (v,e)) args in
       let bindings =
         List.map (fun (v, e) ->
             (<:patt< $lid:v$ >>, e, <:vala< [] >>)
           ) vars_args in
       let newargs =
         List.map (fun (v, _) -> <:expr< $lid:v$ >>) vars_args in
       let body = Expr.applist f newargs in
       <:expr< let $list:bindings$ in $body$ >>
    | e ->  old_migrate_expr dt e
  in
  let dt = { (dt) with migrate_expr = migrate_expr } in
  dt.migrate_expr dt e

let install () = 
let ef = EF.mk () in 
let ef = EF.{ (ef) with
            expr = extfun ef.expr with [
    z ->
    fun arg fallback ->
      Some (lreval z)
  ] } in
  Pa_passthru.(install { name = "pa_lefteval"; ef =  ef ; pass = None ; before = [] ; after = ["pa_deriving"] })
;;

install();;
