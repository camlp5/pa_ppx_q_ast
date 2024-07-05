(**pp -syntax camlp5o -package pa_ppx.import *)
[%%import: Types.t5
 [@add type t4 = [%import: Types.t4]]
 [@add type t4' = [%import: Types.t4']]
][@@deriving quotation_test {
        test_types = [t5]
      ; per_constructor_expansion = [
          (A, AddDel (
                  [],
                  [
                    Types.A (U 1, U 2);
                    Types.A (U 2, U 1)
                  ]
          ))
        ]
      ; expand_types = [
          ([%typ: t4] , Explicit [U 1; U 2])
        ; ([%typ: t4'], Auto)
        ]
  }]
