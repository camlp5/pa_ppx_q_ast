(**pp -syntax camlp5o $(IMPORT_OCAMLCFLAGS) -package pa_ppx.import *)
[%%import: Types.t8
][@@deriving quotation_test {
        test_types = [t8]
  }]
