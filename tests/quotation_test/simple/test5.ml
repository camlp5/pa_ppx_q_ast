(**pp -syntax camlp5o -package pa_ppx.import *)
[%%import: Types.t5
 [@add type t4 = [%import: Types.t4]]
 [@add type t4' = [%import: Types.t4']]
][@@deriving quotation_test {
        test_types = [t5]
      ; expand_types = [
          ([%typ: t4], Auto)
        ; ([%typ: t4'], Auto)
        ]
  }]
