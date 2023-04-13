(**pp -syntax camlp5o $(IMPORT_OCAMLCFLAGS) -package pa_ppx.import *)
[%%import: Types.t10
]
[@@deriving quotation_test {
        test_types = [t10]
  }]
