(* camlp5o *)
(* hCLam.ml *)

module Ploc = Sexp.Ploc

[%%import: Sexp.sexp]
[@@deriving unique { uniqified_module_name = UN
                   ; normal_module_name = OK
                   ; pertype_customization = {
                       sexp = {
                         unique_constructor = sexp
                       }
                     }
                   }]
