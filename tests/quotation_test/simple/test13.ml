(**pp -syntax camlp5o -package pa_ppx.import,pa_ppx_q_ast_quotation_test,pa_ppx_q_ast_test_cleanup,pa_ppx_q_ast_test_renumber *)
[%%import: Types.T13.t13
]
[@@deriving quotation_test {
        test_types = [t13]
      ; minimal_record_module_labels = true
      ; expand_types = [
          ([%typ: t13b], Auto)
        ]
      ; type_module_map = {
          t13 = T13
        ; t13b = T13
        }
  }]
