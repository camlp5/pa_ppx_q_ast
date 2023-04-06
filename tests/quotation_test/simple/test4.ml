(**pp -syntax camlp5o $(IMPORT_OCAMLCFLAGS) -package pa_ppx.import *)
[%%import: Types.t4
][@@deriving quotation_test {
        test_types = [t4]
      ; per_constructor_exprs = [
          (U, [
             (Types.U 1)
           ; (Types.U 2)
           ])
        ]
  }]
