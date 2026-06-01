(**pp -syntax camlp5o -package pa_ppx.import,pa_ppx_migrate,camlp5.quotations,camlp5.extfun *)
(* camlp5o *)
(* pa_string.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Pa_ppx_base
open Pa_ppx_utils
open Pa_passthru
open Ppxutil

let pp_str_item pps ty = Fmt.(pf pps "#<str_item< %s >>" (Eprinter.apply Pcaml.pr_str_item Pprintf.empty_pc ty))

let drop_duplicates el =
  let ht = Hashtbl.create 23 in
  let canon e = Reloc.str_item (fun _ -> Ploc.dummy) 0 e in
  let rec drec acc = function
      [] -> List.rev acc
    | h::tl when Hashtbl.mem ht (canon h) -> drec acc tl
    | h::tl ->
       Hashtbl.add ht (canon h) () ;
       drec (h::acc) tl
  in drec [] el

let dedup_structure arg sil =
  drop_duplicates sil

let dedup_implem arg (si_loc_l, st) =
  let sil = List.map fst si_loc_l in
  let sil = dedup_structure arg sil in
  (List.map (fun si -> (si, MLast.loc_of_str_item si)) sil, st)

let install () = 
let ef = EF.mk () in 
let ef = EF.{ (ef) with
            implem = extfun ef.implem with [
    z ->
    fun arg fallback ->
      Some (dedup_implem arg z)
  ] } in
  Pa_passthru.(install { name = "pa_test_dedup"; ef =  ef ; pass = None ; before = [] ; after = ["pa_deriving"; "pa_test_cleanup"; "pa_test_renumber";"pa_quotation_test"] })
;;

install();;
