(**pp -syntax camlp5o -package pa_ppx.import,pa_ppx_q_ast_quotation_test,pa_ppx_q_ast_test_cleanup,pa_ppx_q_ast_test_renumber *)
[%%import: Types.t9
 [@add type num = [%import: Types.num]]
 [@add type 'a located = [%import: 'a Types.located]]
][@@deriving quotation_test {
        test_types = [t9]
      ; minimal_record_module_labels = true
      ; expand_types = [
          ([%typ: located], Auto)
        ; ([%typ: num], Explicit[0;1])
        ]
  }]
