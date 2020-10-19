(* camlp5o *)
(* hCLam.ml *)

let preeq_option f x y = match (x,y) with
    (None, None) -> true
  | (Some x, Some y) -> f x y
  | _ -> false
let prehash_option f x =
  Hashtbl.hash (Option.map f x)
let hash_option = prehash_option

let preeq_list f l1 l2 =
  List.length l1 = List.length l2 &&
  List.for_all2 f l1 l2

let prehash_list f l =
  Hashtbl.hash (List.map f l)
let hash_list = prehash_list

let _preeq_vala f x y =
  match (x,y) with
    (Ploc.VaAnt s1, Ploc.VaAnt s2) -> s1=s2
  | (Ploc.VaVal v1, Ploc.VaVal v2) -> f v1 v2
  | _ -> false

let _prehash_vala f x =
  match x with
    Ploc.VaAnt s -> Hashtbl.hash s
  | Ploc.VaVal v -> f v


[%%import: Camlp5_ast.expr]
[@@deriving hashcons { hashconsed_module_name = HC
                     ; normal_module_name = OK
                     ; memo = {
                         memo_expr = [%typ: expr]
                       }
                     ; external_types = {
                         Ploc.t = {
                           preeq = (fun x y -> x = y)
                         ; prehash = (fun x -> Hashtbl.hash x)
                         }
                       ; Ploc.vala = {
                           preeq = _preeq_vala
                         ; prehash = _prehash_vala
                         }
                       }
                     ; skip_types = [
                         loc
                       ; longid_lident
                       ; attribute
                       ; attributes_no_anti
                       ; attributes
                       ; case_branch
                       ]
                     }]

external loc_of_hcexpr_node : HC.expr_node -> loc = "%field0"
let loc_of_hcexpr e = loc_of_hcexpr_node e.Hashcons.node
