(**pp -syntax camlp5o $(IMPORT_OCAMLCFLAGS) -package pa_ppx.import *)
[%%import: Types.t
][@@deriving quotation_test {
        test_types = [t]
  }]
