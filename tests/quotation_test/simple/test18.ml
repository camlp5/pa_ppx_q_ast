(**pp -syntax camlp5o -package pa_ppx.import,pa_ppx_q_ast_quotation_test.full *)
[%%import: Types.t18]
[@@deriving quotation_test {
        test_types = [t18]
      ; expand_types_per_type = {
          t18 = [
            ([%typ: string list], Explicit [
                                      l
                                    ; []
            ])
          ]
        }
      ; minimal_record_module_labels = true
  }]
