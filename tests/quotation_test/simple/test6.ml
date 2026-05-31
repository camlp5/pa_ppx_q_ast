(**pp -syntax camlp5o -package pa_ppx.import,pa_ppx_q_ast_quotation_test,pa_ppx_q_ast_test_cleanup,pa_ppx_q_ast_test_renumber *)
[%%import: Types.t6
 [@add type t4 = [%import: Types.t4]]
][@@deriving quotation_test {
        test_types = [t6]
  }]
