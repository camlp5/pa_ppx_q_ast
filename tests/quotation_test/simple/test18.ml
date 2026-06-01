(**pp -syntax camlp5o -package pa_ppx.import,pa_ppx_q_ast_quotation_test.full *)
[%%import: Types.t18_signature_item]
[@@deriving quotation_test {
        test_types = [t18_signature_item]
      ; expand_types = [
          ([%typ: t18_signature_item_desc], Auto)
        ; ([%typ: t18_value_description], Auto)
        ]
      ; expand_types_per_type = {
          t18_value_description = [
            ([%typ: string list], Explicit [
                                      l
                                    ; []
            ])
          ]
        }
      ; expand_types_per_constructor = [
          (Psig_value,
           [ ([%typ: string list],
              Explicit [
                  l
                ; []
             ])
           ])
        ]
      ; minimal_record_module_labels = true
  }]
