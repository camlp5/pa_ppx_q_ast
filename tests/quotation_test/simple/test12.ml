(**pp -syntax camlp5o -package pa_ppx.import *)
[%%import: Types.t12
]
[@@deriving quotation_test {
        test_types = [t12]
      ; minimal_record_module_labels = true
      ; expand_types = {
          t12' = AddDel (
                     [],
                     [Types.B]
                   )
        }
  }]
