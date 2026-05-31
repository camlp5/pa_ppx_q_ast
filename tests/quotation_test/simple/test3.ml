(**pp -syntax camlp5o -package pa_ppx.import,pa_ppx_q_ast_quotation_test.full *)
[%%import: Types.t3b
  [@add [%%import: Types.located]]
][@@deriving quotation_test {
        test_types = [t3b]
      ; minimal_record_module_labels = true
      ; expand_types_per_constructor = [
          (X, [ ([%typ: t3a], Auto) ])
        ; (Z, [ ([%typ: t3a], Auto) ; ([%typ: located], Auto) ])
        ]
  }]
