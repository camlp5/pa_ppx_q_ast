(** -syntax camlp5o  -package pa_ppx_q_ast,pa_ppx.import,pa_ppx_hashcons $(IMPORT_OCAMLCFLAGS) *)
(* camlp5o *)
(* hCLam.ml *)

[%%import: Lam.lam]
[@@deriving hashcons { hashconsed_module_name = HC
                     ; normal_module_name = OK
                     ; memo = {
                         memo_lam = [%typ: lam]
                       ; memo_int_lam = [%typ: int * lam]
                       ; memo_int = [%typ: int]
                       }
                     ; external_types = {
                         Ploc.t = {
                           preeq = (fun x y -> x = y)
                         ; prehash = (fun x -> Hashtbl.hash x)
                         }
                       ; Ploc.vala = {
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
                         lam = {
                           hashcons_module = Lam
                         ; hashcons_constructor = lam
                         }
                       }
                     }]
