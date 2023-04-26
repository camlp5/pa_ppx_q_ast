(**pp -syntax camlp5o $(IMPORT_OCAMLCFLAGS) -package pa_ppx.import *)
[%%import: Types.t11
]
[@@deriving quotation_test {
        test_types = [t11]
      ; minimal_record_module_labels = true
      ; prefix_of_type = [
          ([%typ: (int * (bool * char)) list], lt)
        ]
  }]
