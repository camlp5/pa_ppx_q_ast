(**pp -syntax camlp5o $(IMPORT_OCAMLCFLAGS) -package pa_ppx.import *)
[%%import: Type1.t4
][@@deriving quotation_test {
        test_types = [t4]
      ; per_constructor_exprs = [
          (U, [
             (Type1.U 1)
           ; (Type1.U 2)
           ])
        ]
  }]
