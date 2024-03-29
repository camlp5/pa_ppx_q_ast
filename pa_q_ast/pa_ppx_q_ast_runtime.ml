(**pp -syntax camlp5r -package camlp5.parser_quotations *)
module MetaE = struct
  include Q_ast_base.E_MetaSig ;
  value app_no_loc ?{prefix} fid el =
    let prefix = match prefix with [ None -> <:longident< MLast >> | Some p -> p ] in
    List.fold_left (fun e1 e2 -> <:expr< $e1$ $e2$ >>)
      <:expr< $longid:prefix$ . $lid:fid$ >> el
  ;
end
;

module MetaP = struct
  include Q_ast_base.P_MetaSig ;
end
;

value separate_locate s =
  let len = String.length s in
  if len > 0 && s.[0] = '@' then (String.sub s 1 (len - 1), True)
  else (s, False)
;

value insert_loc_variable ast =
  let (p, pl) =
    loop [] ast where rec loop pl =
    fun
      [ <:patt:< $p1$ $p2$ >> -> loop [(p2, loc) :: pl] p1
      | p -> (p, pl) ]
  in
  match pl with
    [ [(<:patt< _ >>, loc) :: pl] ->
      List.fold_left (fun p1 (p2, loc) -> <:patt< $p1$ $p2$ >>)
        <:patt< $p$ $lid:Ploc.name.val$ >> pl
    | _ -> ast ]
;


value parse_string grammar_entry from_string s =
  match (grammar_entry, from_string) with [
      (None, None) | (Some _, Some _) -> assert False
      | (Some e,_) ->
         Ploc.call_with Plexer.force_antiquot_loc True
           (Grammar.Entry.parse e) (Stream.of_string s)
      | (None, Some pf) -> pf s
    ]
;

value apply_entry grammar_entry from_string me mp =
  let f s = parse_string grammar_entry from_string s in
  let expr s =
    let (s, locate) = separate_locate s in
    me () (f s)
  in
  let patt s =
    let (s, locate) = separate_locate s in
    let ast = mp () (f s) in
    if locate then
      let (p, pl) =
        loop [] ast where rec loop pl =
          fun
          [ <:patt:< $p1$ $p2$ >> -> loop [(p2, loc) :: pl] p1
          | p -> (p, pl) ]
      in
      match (p,pl) with
      [ (_, [(<:patt< _ >>, loc) :: pl]) ->
          List.fold_left (fun p1 (p2, loc) -> <:patt< $p1$ $p2$ >>)
            <:patt< $p$ $lid:Ploc.name.val$ >> pl
      | (<:patt:< ( $list:pl$ ) >>, []) ->
        let pl = match pl with [
          [ <:patt< _ >> :: pl ] -> [ <:patt< $lid:Ploc.name.val$ >> :: pl ]
        | _ -> pl
        ] in
        <:patt< ( $list:pl$ ) >>
      | _ -> ast ]
    else ast
  in
  Quotation.ExAst (expr, patt)
;

module Locate = struct
  type t = { ctr : ref int ; locate : bool } ;
  value mk locate = { ctr = ref 0 ; locate = locate } ;
  value locate r = r.locate ;
  value get r = do {
    let v = r.ctr.val in
    incr r.ctr ;
    v
  }
  ;
end ;

value customloc_apply_entry grammar_entry from_string me mp =
  let f s = parse_string grammar_entry from_string s in
  let expr s =
    let (s, locate) = separate_locate s in
    me () (f s)
  in
  let patt s =
    let (s, locate) = separate_locate s in
    mp (Locate.mk locate) (f s)
  in
  Quotation.ExAst (expr, patt)
;

value noloc_apply_entry grammar_entry from_string me mp =
  let f s = parse_string grammar_entry from_string s in
  let expr s =
    let (s, locate) = separate_locate s in
    me () (f s)
  in
  let patt s =
    let (s, locate) = separate_locate s in
    mp () (f s)
  in
  Quotation.ExAst (expr, patt)
;

value hc_apply_entry grammar_entry from_string me mp =
  let f s = parse_string grammar_entry from_string s in
  let expr s =
    let (s, locate) = separate_locate s in
    me () (f s)
  in
  let patt s =
    let (s, locate) = separate_locate s in
    let ast = mp () (f s) in
    match (locate, ast) with [
      (True, <:patt:< {Hashcons.node = $ast$} >>) ->
      let ast = insert_loc_variable ast in
      <:patt< {Hashcons.node = $ast$} >>
    | (True, _) -> insert_loc_variable ast
    | (False, _) -> ast
    ] in
  Quotation.ExAst (expr, patt)
;

value customloc_hc_apply_entry grammar_entry from_string me mp =
  let f s = parse_string grammar_entry from_string s in
  let expr s =
    let (s, locate) = separate_locate s in
    me () (f s)
  in
  let patt s =
    let (s, locate) = separate_locate s in
    mp (Locate.mk locate) (f s)
 in
  Quotation.ExAst (expr, patt)
;

value unique_apply_entry grammar_entry from_string me mp =
  let f s = parse_string grammar_entry from_string s in
  let expr s =
    let (s, locate) = separate_locate s in
    me () (f s)
  in
  let patt s =
    let (s, locate) = separate_locate s in
    let ast = mp () (f s) in
    match (locate, ast) with [
      (True, (<:patt:< {Pa_ppx_unique_runtime.Unique.node = $ast$} >> | <:patt:< {Unique.node = $ast$} >>)) ->
      let ast = insert_loc_variable ast in
      <:patt< {Pa_ppx_unique_runtime.Unique.node = $ast$} >>
    | (True, _) -> insert_loc_variable ast
    | (False, _) -> ast
    ] in
  Quotation.ExAst (expr, patt)
;

value customloc_unique_apply_entry grammar_entry from_string me mp =
  let f s = parse_string grammar_entry from_string s in
  let expr s =
    let (s, locate) = separate_locate s in
    me () (f s)
  in
  let patt s =
    let (s, locate) = separate_locate s in
    mp (Locate.mk locate) (f s)
  in
  Quotation.ExAst (expr, patt)
;
