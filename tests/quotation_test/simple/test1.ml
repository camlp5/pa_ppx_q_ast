(**pp -syntax camlp5o -ppopt -pa_ppx_q_ast.quotation_test-test-type -ppopt t *)
[%%quotation_test type t = Types.t = A | B]
