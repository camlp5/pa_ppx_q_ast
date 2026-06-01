(**pp -syntax camlp5o -package pa_ppx.import,pa_ppx_migrate,camlp5.quotations,camlp5.extfun *)
(* camlp5o *)
(* pa_string.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Pa_ppx_base
open Pa_ppx_utils
open Pa_passthru
open Ppxutil

let pp_str_item pps ty = Fmt.(pf pps "#<str_item< %s >>" (Eprinter.apply Pcaml.pr_str_item Pprintf.empty_pc ty))

exception Migration_error of string

let migration_error feature =
  raise (Migration_error feature)

let mapLR f l =
  let rec mrec acc = function
      [] -> List.rev acc
    | h::t ->
       let h = f h in
       mrec (h::acc) t
  in mrec [] l

let _migrate_list subrw0 __dt__ l =
  mapLR (subrw0 __dt__) l

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

let renumber_variables varname loc_varname e =
  let ctr = ref 0 in
  let mkid () =
    let n = !ctr in
    ctr := n+1 ;
    Printf.sprintf "%s%02d" varname n
  in
  let dt =
    let dt = Migrate_camlp5.make_dt() in
    let old_migrate_expr = dt.migrate_expr in
    let migrate_expr dt = function
        <:expr:< $lid:v$ >> when v <> loc_varname ->
        let v = mkid () in
        <:expr< $lid:v$ >>
      | e -> old_migrate_expr dt e
    in
    { (dt) with migrate_expr = migrate_expr } in
    
  dt.migrate_expr dt e

let renumber_str_item =
  function
      <:str_item:< $exp:e$ >> ->
      Some <:str_item< $exp:renumber_variables "x" "loc" e$ >>
    | <:str_item< [@@@ $attribute:_$ ] >> as si -> Some si
    | si ->
       Fmt.(raise_failwithf (MLast.loc_of_str_item si) "pa_ppx_q_ast.test_renumber: unrecognized extension payload:\n@[%a@]"
              pp_str_item si) 

let renumber_str_item arg (acc, (varname, loc_varname)) = function
    <:str_item:< $exp:e$ >> ->
      (<:str_item< $exp:renumber_variables varname loc_varname e$ >>::acc, (varname, loc_varname))
  | <:str_item:< [@@@"pa_ppx_q_ast.test_renumber.params" { varname = $str:v$; loc_varname = $str:lv$ } ;] >> ->
     (acc, (v, lv))
  | si -> (si::acc, (varname, loc_varname))

let renumber_structure arg sil =
  let loc_varname = "loc" in
  let varname = "x" in
  let (rev_sil, _) = List.fold_left (renumber_str_item arg) ([], (varname, loc_varname)) sil in
  List.rev rev_sil

let renumber_implem arg (si_loc_l, st) =
  let sil = List.map fst si_loc_l in
  let sil = renumber_structure arg sil in
  (List.map (fun si -> (si, MLast.loc_of_str_item si)) sil, st)

let install () = 
let ef = EF.mk () in 
let ef = EF.{ (ef) with
            implem = extfun ef.implem with [
    z ->
    fun arg fallback ->
      Some (renumber_implem arg z)
  ] } in
  Pa_passthru.(install { name = "pa_test_renumber"; ef =  ef ; pass = None ; before = [] ; after = ["pa_deriving"; "pa_test_cleanup";"pa_quotation_test"] })
;;

install();;
