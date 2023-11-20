(**pp -syntax camlp5o -package pa_ppx.import *)
[%%import: Types.t8
][@@deriving quotation_test {
        test_types = [t8]
  }]
