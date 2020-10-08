
module MetaE = struct
  include Q_ast_base.E_MetaSig ;
  value app_no_loc ?{prefix} fid el =
    let prefix = match prefix with [ None -> <:expr< MLast >> | Some p -> p ] in
    List.fold_left (fun e1 e2 -> <:expr< $e1$ $e2$ >>)
      <:expr< $prefix$ . $lid:fid$ >> el
  ;

  value int n = let loc = Ploc.dummy in <:expr< $int:string_of_int n$ >> ;
end
;

module MetaP = struct
  include Q_ast_base.P_MetaSig ;
  value int n = let loc = Ploc.dummy in <:patt< $int:string_of_int n$ >> ;
end
;
