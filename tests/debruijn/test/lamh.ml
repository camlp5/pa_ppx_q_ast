(**pp -syntax camlp5o -package hashcons,debruijn_example.parser_quotations *)
(* programme de test de réductions massives de lambda-termes *)
(* version AVEC hash-consing *)

open Debruijn_hashcons.HC
open Hashcons

let lift n = 
  let rec lift_rec k = 
    let rec lift_k t = match t with
    | <:hcdebruijn< $ref:i$ >> -> 
	if i<k then (*Ref(i)*)t (* bound variables are invariant *)
	else <:hcdebruijn< $ref:n+i$ >>    (* free variables are relocated by n *)
    | <:hcdebruijn< []$term:t$ >>   -> <:hcdebruijn< []$term:lift_rec (k+1) t$ >>
    | <:hcdebruijn< $term:t$ $term:u$ >> -> <:hcdebruijn< $term:lift_k t$ $term:lift_k u$ >>
    in
    lift_k
  in
  lift_rec 0 

let lift = memo2_int_term lift

let subst_count = Stdlib.ref 0

let subst w = 
  incr subst_count;
  let rec subst_w n t = match t with
    | <:hcdebruijn< $ref:k$ >> -> 
	if k=n then lift n w      (* substituted variable *)
	else if k<n then (*Ref k*)t    (* bound variables *)
        else <:hcdebruijn< $ref:k-1$ >>       (* free variables *)
    | <:hcdebruijn< []$term:t$ >>   -> <:hcdebruijn< []$term:subst_w (n+1) t$ >>
    | <:hcdebruijn< $term:t$ $term:u$ >> -> <:hcdebruijn< $term:subst_w n t$ $term:subst_w n u$ >>
  in 
  subst_w 0

let subst = memo2_term_term subst

let rec hnf t = match t with
  | <:hcdebruijn< $ref:n$ >> -> t
  | <:hcdebruijn< []$term:t$ >>   -> <:hcdebruijn< []$term:hnf t$ >>
  | <:hcdebruijn< $term:t$ $term:u$ >> -> match hnf t with
      | <:hcdebruijn< []$term:w$ >> -> hnf (subst u w)
      | h     -> <:hcdebruijn< $term:h$ $term:u$ >>

(*let nhf = memo hnf*)

let rec nf t = match t with
  | <:hcdebruijn< $ref:n$ >> -> t
  | <:hcdebruijn< []$term:t$ >>   -> <:hcdebruijn< []$term:nf t$ >>
  | <:hcdebruijn< $term:t$ $term:u$ >> -> match hnf t with
      | <:hcdebruijn< []$term:w$ >>  -> nf (subst u w)
      | h      -> <:hcdebruijn< $term:nf h$ $term:nf u$ >>

(*let nf = memo nf*)

type expr = Ref2 of int | Abs2 of expr | App2 of expr * expr
let rec term_of_expr = function
  | Ref2 i -> <:hcdebruijn< $ref:i$ >>
  | Abs2 t -> <:hcdebruijn< []$term:term_of_expr t$ >>
  | App2 (u,v) -> <:hcdebruijn< $term:term_of_expr u$ $term:term_of_expr v$ >>
let quicksort = 
  let c = open_in "quicksort.term" in
  let e = (input_value c : expr) in
  close_in c; 
  term_of_expr e

let nil = (*[c,n]n*) (*abs (abs (ref 0))*) <:hcdebruijn< [][]0 >>
let cons = (*[x,l][c,n](c x (l c n))*)
(*
  abs(abs(abs(abs(app(app (ref 1, 
			   ref 3),
		      app (app (ref 2, 
				ref 1), 
			   ref 0))))))
*)
  <:hcdebruijn< [][][][](1 3 (2 1 0)) >>
let zero = (*[s,z]z*) (*abs (abs (ref 0)) *) <:hcdebruijn< [][]0 >>
let succ = (*[n][s,z](s (n s z))*)
(*
  abs(abs(abs(app (ref 1,
		   app (app (ref 2, ref 1), ref 0)))))
*)
  <:hcdebruijn< [][][](1 (2 1 0)) >>

let rec iter f n x = if n=0 then x else iter f (n-1) (f x)

(* Church *)
let church n = iter (fun c -> nf (<:hcdebruijn< $term:succ$ $term:c$ >>)) n zero

(* list : int list -> term *)
let rec list = function
  | x :: l -> 
      let cx = church x and ll = list l in 
      (*[c,n](c ^Cx (^Ll c n))*) 
(*
      abs(abs(app (app (ref 1, cx),
		   app (app (ll, ref 1), ref 0))))
*)
      <:hcdebruijn< [][](1 $term:cx$ ($term:ll$ 1 0)) >>
  | []   -> nil


(* and back *)

let eval_nat iter init = function
  | <:hcdebruijn< [][]$term:t$ >> (* [s,z]t *) -> 
      let rec eval_rec = function
        | (* z *) <:hcdebruijn< 0 >> -> init
        | (* (s u) *) <:hcdebruijn< 1 $term:u$ >> -> iter (eval_rec u)
        | _ -> failwith "Not a normal church natural"
        in
	eval_rec t
  | _ -> failwith "Not a normal church natural"

let compute_nat = eval_nat (fun n->n+1) 0
 
let normal_nat n = compute_nat (nf n)

let eval_list_of_nats = function
  | <:hcdebruijn< [][]$term:t$ >> (* [c,n]t *) -> 
      let rec lrec = function
        | (* n *)       <:hcdebruijn< 0 >>                   -> []
        | (* (c x l) *) <:hcdebruijn< 1 $term:x$ $term:l$ >> -> 
	    (compute_nat x) :: (lrec l)
        | _ -> failwith "Not a normal List"
      in
      lrec t
  | _ -> failwith "Not a noraml List"


let normal_list l = eval_list_of_nats (nf l)

(* bench *)

open Format

(*let () = Gc.set { (Gc.get()) with Gc.verbose = 0x00d }*)

let () =
  let l = list [0;3;5;2;4;1] in 
  assert (normal_list <:hcdebruijn< $term:quicksort$ $term:l$ >> = [0;1;2;3;4;5]);
  printf "subst count: %d@." !subst_count;
  let stat = Gc.stat () in
  printf "top heap words: %d (%d kb)@." stat.Gc.top_heap_words
    (stat.Gc.top_heap_words / 256);
  let l,n,s,b1,b2,b3 = Term.stats term_ht in
  printf "table length: %d / nb. entries: %d / sum of bucket length: %d@."
    l n s;
  printf "smallest bucket: %d / median bucket: %d / biggest bucket: %d@."
    b1 b2 b3



