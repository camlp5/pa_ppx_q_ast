(** -syntax camlp5r *)
module MetaE = struct
  include Q_ast_base.E_MetaSig ;
  value app_no_loc ?{prefix} fid el =
    let prefix = match prefix with [ None -> <:longident< MLast >> | Some p -> p ] in
    List.fold_left (fun e1 e2 -> <:expr< $e1$ $e2$ >>)
      <:expr< $longid:prefix$ . $lid:fid$ >> el
  ;

  value int n = let loc = Ploc.dummy in <:expr< $int:string_of_int n$ >> ;
end
;

module MetaP = struct
  include Q_ast_base.P_MetaSig ;
  value int n = let loc = Ploc.dummy in <:patt< $int:string_of_int n$ >> ;
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


value apply_entry e me mp =
  let f s =
    Ploc.call_with Plexer.force_antiquot_loc True
      (Grammar.Entry.parse e) (Stream.of_string s)
  in
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

module Fresh = struct
  type t = ref int  ;
  value mk () = ref 0 ;
  value get r = do {
    let v = r.val in
    incr r ;
    v
  }
  ;
end ;

value customloc_apply_entry e me mp =
  let f s =
    Ploc.call_with Plexer.force_antiquot_loc True
      (Grammar.Entry.parse e) (Stream.of_string s)
  in
  let expr s =
    let (s, locate) = separate_locate s in
    me () (f s)
  in
  let patt s =
    let (s, locate) = separate_locate s in
    mp (Fresh.mk()) (f s)
  in
  Quotation.ExAst (expr, patt)
;

value noloc_apply_entry e me mp =
  let f s =
    Ploc.call_with Plexer.force_antiquot_loc True
      (Grammar.Entry.parse e) (Stream.of_string s)
  in
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

value hc_apply_entry e me mp =
  let f s =
    Ploc.call_with Plexer.force_antiquot_loc True
      (Grammar.Entry.parse e) (Stream.of_string s)
  in
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

value unique_apply_entry e me mp =
  let f s =
    Ploc.call_with Plexer.force_antiquot_loc True
      (Grammar.Entry.parse e) (Stream.of_string s)
  in
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
