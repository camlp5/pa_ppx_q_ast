(* camlp5o *)
(* hCLam.ml *)

[%%import: Sexp.sexp]
[@@deriving hashcons { hashconsed_module_name = HC
                     ; normal_module_name = OK
                     ; external_types = {
                         Ploc.vala = {
                           preeq = (fun f x y -> match (x,y) with
                               (Ploc.VaAnt s1, Ploc.VaAnt s2) -> s1=s2
                             | (Ploc.VaVal v1, Ploc.VaVal v2) -> f v1 v2
                             | _ -> false
                             )
                         ; prehash = (fun f x -> match x with
                             Ploc.VaAnt s -> Hashtbl.hash s
                           | Ploc.VaVal v -> f v
                           )
                         }
                       }
                     ; pertype_customization = {
                         sexp = {
                           hashcons_constructor = sexp
                         }
                       }
                     }]
