(**pp -syntax camlp5o -package pa_ppx.import *)
[%%import: Types.t7
 [@add type t3a = [%import: Types.t3a]]
][@@deriving quotation_test {
        test_types = [t7; t7']
      ; expand_types_per_type = {
          t7' = {
            t3a = Auto
          }
        ; t7 = {
            t7 = Explicit [Types.A U]
          }
        }
  }]
