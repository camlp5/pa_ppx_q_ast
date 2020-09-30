(* camlp5o *)
(* hCLam.ml *)

[%%import: Lam.lam]
[@@hashcons_module Lam][@@hashcons_constructor lam]
[@@deriving hashcons { module_name = HC
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
                             )
                         ; prehash = (fun f x -> match x with
                             Ploc.VaAnt s -> Hashtbl.hash s
                           | Ploc.VaVal v -> f v
                           )
                         }
                       ; MLast.type_var = {
                           preeq = (fun x y -> x = y)
                         ; prehash = (fun x -> Hashtbl.hash x)
                         }
                       }
                     }]
