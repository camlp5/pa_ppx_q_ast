(**pp -syntax camlp5r -package pa_ppx_q_ast_test_renumber *)
[@@@"pa_ppx_q_ast.test_renumber.params" { varname = "x"; loc_varname = "__loc__" };];
[@@@"ocaml.text" "t2";];
{Types.f1 = {Types.txt = v; loc = __loc__}};
[@@@"ocaml.text" "located";];
[@@@"ocaml.text" "location";];
[@@@"pa_ppx_q_ast.test_renumber.params" { varname = "y"; loc_varname = "loc" };];
{Types.f1 = {Types.txt = v; loc = loc}};
[@@@"ocaml.text" "t5";];
Types.A (Types.U x) (Types.U x);
Types.B (Types.U x) (Types.V x);
[@@@"ocaml.text" "t5b";];
Types.A x x;
Types.B x x;
