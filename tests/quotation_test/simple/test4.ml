(**pp -syntax camlp5o -package pa_ppx.import *)
[%%import: Types.t4
][@@deriving quotation_test {
        test_types = [t4]
      ; per_constructor_expansion = [
          (U, Explicit [
             (Types.U 1)
           ; (Types.U 2)
           ])
        ]
  }]
