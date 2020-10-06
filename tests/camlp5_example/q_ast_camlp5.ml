(* camlp5r *)
(* pa_sexp.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Asttools
open MLast
open Pcaml

open Pa_camlp5
open Q_ast 

module Regular = struct

module MetaE = struct
  include Q_ast_base.E_MetaSig
  let int n = let loc = Ploc.dummy in <:expr< $int:string_of_int n$ >>
end

module MetaP = struct
  include Q_ast_base.P_MetaSig
  let int n = let loc = Ploc.dummy in <:patt< $int:string_of_int n$ >>
end

[%%import: Camlp5_ast.expr]
[@@deriving q_ast {
    data_source_module = Camlp5_ast
  ; expr_meta_module = MetaE
  ; patt_meta_module = MetaP
 }]

end

module OK = struct
module MetaE = Regular.MetaE
module MetaP = Regular.MetaP

[%%import: Camlp5_hashcons.OK.expr]
[@@deriving q_ast {
    data_source_module = Camlp5_hashcons.OK
  ; expr_meta_module = MetaE
  ; patt_meta_module = MetaP
  }]

Quotation.add "okexpr"
  (apply_entry Pa_camlp5.expr_eoi E.expr P.expr)
end

module Hashcons = struct

module MetaE = struct
  include Regular.MetaE
  let app_no_loc ?prefix fid el =
    let prefix = match prefix with None -> <:expr< MLast >> | Some p -> p in
    List.fold_left (fun e1 e2 -> <:expr< $e1$ $e2$ >>)
      <:expr< $prefix$ . $lid:fid$ >> el
end

module MetaP = struct
  include Regular.MetaP
end

[%%import: Camlp5_hashcons.HC.expr]
[@@deriving q_ast {
    data_source_module = Camlp5_hashcons.HC
  ; quotation_source_module = Camlp5_migrate.Project
  ; expr_meta_module = MetaE
  ; patt_meta_module = MetaP
  ; hashconsed = true
  }]

Quotation.add "hcexpr"
  (apply_entry Pa_camlp5.expr_hashcons_eoi E.expr P.expr)
end

