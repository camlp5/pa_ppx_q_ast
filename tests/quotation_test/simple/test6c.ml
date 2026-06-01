(**pp -syntax camlp5o -package pa_ppx.import,pa_ppx_q_ast_quotation_test,pa_ppx_q_ast_test_cleanup,pa_ppx_q_ast_test_renumber *)
[%%import: Types.t6c]
[@@deriving quotation_test {
        target_is_pattern_ast = true
      ; test_types = [t6c]
  }]

[@@@ocaml.text "not-pattern"]
[%%import: Types.t6c]
[@@deriving quotation_test {
        target_is_pattern_ast = false
      ; test_types = [t6c]
  }]
