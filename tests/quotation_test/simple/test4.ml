(**pp -syntax camlp5o $(IMPORT_OCAMLCFLAGS) -package pa_ppx.import *)
[%%import: Type1.t3b
][@@deriving quotation_test {
        test_types = [t3b]
      ; expand_types_per_constructor = [
          (X, [t3a])
        ]
  }]
