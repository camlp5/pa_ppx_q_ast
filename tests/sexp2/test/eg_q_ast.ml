(** -syntax camlp5o -package sexp2_example.parser_quotations *)
(* camlp5o *)
(* eg_sexp.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Fmt ;;

module Regular = struct
let rec pp pps = function
    <:sexp:< () >> -> Fmt.(pf pps "()")
  | <:sexp:< ( $exp:a$ . $exp:b$ ) >> -> Fmt.(pf pps "(%a . %a)" pp a pp b)
  | <:sexp:< $atom:a$ >> -> Fmt.(pf pps "%a" string a)
;;

let subst rho e =
  let rec srec = function
    <:sexp:< () >> -> <:sexp< () >>
  | <:sexp:< ( $exp:a$ . $exp:b$ ) >> -> 
    <:sexp:< ( $exp:srec a$ . $exp:srec b$ ) >>
  | <:sexp:< $atom:a$ >> as z ->
    if List.mem_assoc a rho then List.assoc a rho
    else z
in srec e
;;

let rec atoms = function
    <:sexp:< () >> -> []
  | <:sexp:< $atom:a$ >> -> [a]
  | <:sexp:< ( () . $exp:cdr$ ) >> -> atoms cdr
  | <:sexp:< ( $atom:a$ . $exp:cdr$ ) >> -> a::(atoms cdr)
  | <:sexp:< ( ( $exp:caar$ . $exp:cdar$ ) . $exp:cdr$ ) >> ->
    atoms <:sexp< ( $exp:caar$ . ( $exp:cdar$ . $exp:cdr$ ) ) >>
end
;;
