(* camlp5r *)
(* pa_sexp.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Asttools
open MLast
open Pcaml

open Pa_debruijn
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

[%%import: Debruijn.term]
[@@deriving q_ast {
    data_source_module = Debruijn
  ; expr_meta_module = MetaE
  ; patt_meta_module = MetaP
 }]

Quotation.add "debruijn"
  (apply_entry Pa_debruijn.term_eoi E.term P.term)
end

module OK = struct
module MetaE = Regular.MetaE
module MetaP = Regular.MetaP

[%%import: Debruijn_hashcons.OK.term]
[@@deriving q_ast {
    data_source_module = Debruijn_hashcons.OK
  ; expr_meta_module = MetaE
  ; patt_meta_module = MetaP
  }]

Quotation.add "okdebruijn"
  (apply_entry Pa_debruijn.term_eoi E.term P.term)
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

[%%import: Debruijn_hashcons.HC.term]
[@@deriving q_ast {
    data_source_module = Debruijn_hashcons.HC
  ; quotation_source_module = Debruijn_migrate.Project
  ; expr_meta_module = MetaE
  ; patt_meta_module = MetaP
  ; hashconsed = true
  }]

Quotation.add "hcdebruijn"
  (apply_entry Pa_debruijn.term_hashcons_eoi E.term P.term)
end
