(**pp -syntax camlp5o -package pa_ppx.import,pa_ppx_q_ast_quotation_test.full *)
[%%import: Types.t11
]
[@@deriving quotation_test {
        test_types = [t11]
      ; minimal_record_module_labels = true
      ; prefix_of_type = [
          ([%typ: (int * (bool * char)) list], lt)
        ]
  }]
