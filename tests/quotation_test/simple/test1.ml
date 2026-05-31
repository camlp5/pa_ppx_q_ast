(**pp -syntax camlp5o -package pa_ppx.import,pa_ppx_q_ast_quotation_test,pa_ppx_q_ast_test_cleanup,pa_ppx_q_ast_test_renumber -ppopt -pa_ppx_q_ast.quotation_test-test-type -ppopt t *)
[%%quotation_test type t = Types.t = A | B]
