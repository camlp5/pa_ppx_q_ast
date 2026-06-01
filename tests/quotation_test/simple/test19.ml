(**pp -syntax camlp5o -package pa_ppx.import,pa_ppx_q_ast_quotation_test.full *)
[%%typedecls
[%%import: Types.t19_a]
[%%import: Types.t]
]
[@@deriving quotation_test {
        test_types = [t19_a; t19_b]
      ; expand_test_types = [
          ([%typ: t19_b], DelPatts [
                              [%patt? Types.T19_A]
          ])
        ]
      ; expand_types = [
          ([%typ: t], Auto)
        ]
      ; minimal_record_module_labels = true
      ; type_module_map = {
          t19_a = Types
        ; t = Types
        }
  }]
