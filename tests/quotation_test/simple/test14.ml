(**pp -syntax camlp5o -package pa_ppx.import *)
[%%import: Types.T14.t14
]
[@@deriving quotation_test {
        test_types = [t14]
      ; minimal_record_module_labels = true
      ; type_module_map = {
          t14 = T14
        }
  }]
