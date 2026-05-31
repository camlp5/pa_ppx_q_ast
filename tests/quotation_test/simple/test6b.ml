(**pp -syntax camlp5o -package pa_ppx.import,pa_ppx_q_ast_quotation_test,pa_ppx_q_ast_test_cleanup,pa_ppx_q_ast_test_renumber *)
[%%typedecls
[%%import: Types.t]
[%%import: Types.t6b]
]
[@@deriving quotation_test {
        target_is_pattern_ast = true
      ; test_types = [t6b]
      ; expand_types = [
          ([%typ: t], Auto)
        ]
  }]

[@@@ocaml.text "not-pattern"]
[%%typedecls
[%%import: Types.t]
[%%import: Types.t6b]
]
[@@deriving quotation_test {
        target_is_pattern_ast = false
      ; test_types = [t6b]
      ; expand_types = [
          ([%typ: t], Auto)
        ]
  }]
