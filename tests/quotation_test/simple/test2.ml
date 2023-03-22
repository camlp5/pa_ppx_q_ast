(**pp -syntax camlp5o $(IMPORT_OCAMLCFLAGS) -package pa_ppx.import *)
[%%import: Type1.t
][@@deriving quotation_test {
        test_types = [t]
  }]
