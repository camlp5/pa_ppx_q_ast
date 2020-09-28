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

value canon_expr e = Reloc.expr (fun _ -> Ploc.dummy) 0 e ;
value canon_ctyp ty = Reloc.ctyp (fun _ -> Ploc.dummy) 0 ty ;
value builtin_types =
  let loc = Ploc.dummy in
  List.map canon_ctyp [
    <:ctyp< string >>
  ; <:ctyp< int >>
  ; <:ctyp< int32 >>
  ; <:ctyp< int64 >>
  ; <:ctyp< nativeint >>
  ; <:ctyp< float >>
  ; <:ctyp< bool >>
  ; <:ctyp< char >>
  ]
;

module QAST = struct
type t = {
  source_module_name : string
; type_decls : list (string * MLast.type_decl)
}
;

value build_context loc ctxt tdl =
  let type_decls = List.map (fun (MLast.{tdNam=tdNam} as td) ->
      (tdNam |> uv |> snd |> uv, td)
    ) tdl in
  let open Ctxt in
  let source_module_name = match option ctxt "source_module_name" with [
    <:expr< $uid:mname$ >> -> mname
  | _ -> Ploc.raise loc (Failure "pa_deriving_q_ast: option source_module_name must be a UIDENT")
  | exception Failure _ ->
  Ploc.raise loc (Failure "pa_deriving_q_ast: option source_module_name must be specified")
  ] in
  {
    source_module_name = source_module_name
  ; type_decls = type_decls
  }
;

value generate_meta_e_bindings arg rc l = [] ;
value generate_meta_p_bindings arg rc l = [] ;

end
;

value str_item_gen_q_ast name arg = fun [
  <:str_item:< type $_flag:_$ $list:tdl$ >> ->
    let rc = QAST.build_context loc arg tdl in
    let meta_e_bindings = List.concat (List.map (QAST.generate_meta_e_bindings arg rc) rc.QAST.type_decls) in
    let meta_p_bindings = List.concat (List.map (QAST.generate_meta_p_bindings arg rc) rc.QAST.type_decls) in
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
; options = ["optional"; "source_module_name"]
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

