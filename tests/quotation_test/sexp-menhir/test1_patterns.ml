(**pp -syntax camlp5r -package sexp_menhir_example.parser_quotations *)
[@@@"pa_ppx_q_ast.test_renumber.params" {varname = "x"; loc_varname = "__loc__"};];
[@@@"ocaml.text" "sexp";];
<:sexp< $atom:s$ >> ;
<:sexp< ($x1$ . $x2$) >>;
<:sexp< () >> ;
