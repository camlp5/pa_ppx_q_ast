(**pp $(IMPORT_OCAMLCFLAGS) -package pa_ppx_q_ast,pa_ppx.import,pa_ppx_hashcons -syntax camlp5o *)
(* camlp5o *)
(* hCLam.ml *)

[%%import: Debruijn.term]
[@@deriving hashcons { hashconsed_module_name = HC
                     ; normal_module_name = OK
                     ; memo = {
                         memo_term = [%typ: term]
                       ; memo2_int_term = [%typ: int * term]
                       ; memo2_term_term = [%typ: term * term]
                       ; memo_int = [%typ: int]
                       }
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
                         term = {
                           hashcons_module = Term
                         ; hashcons_constructor = term
                         }
                       }
                     }]
