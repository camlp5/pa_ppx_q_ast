(**pp -syntax camlp5o -package pa_ppx.import,pa_ppx_q_ast_quotation_test,pa_ppx_q_ast_test_cleanup,pa_ppx_q_ast_test_renumber *)
[%%import: Types.T14.t14
]
[@@deriving quotation_test {
        test_types = [t14]
      ; minimal_record_module_labels = true
      ; type_module_map = {
          t14 = T14
        }
  }]
