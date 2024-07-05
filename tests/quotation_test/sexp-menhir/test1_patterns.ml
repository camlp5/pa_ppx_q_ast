(**pp -syntax camlp5r -package sexp_menhir_example.parser_quotations *)
[@@@"ocaml.text" "sexp";];
<:sexp< $atom:s$ >> ;
<:sexp< ($x1$ . $x2$) >>;
<:sexp< () >> ;
