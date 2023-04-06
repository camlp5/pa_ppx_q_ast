(**pp -syntax camlp5o $(IMPORT_OCAMLCFLAGS) -package pa_ppx.import *)
[%%import: Types.t4
][@@deriving quotation_test {
        test_types = [t4]
      ; expand_types_per_constructor = [
          (U, { num = Explicit [1; 2] })
        ]
  }]
