(**pp -syntax camlp5o -package pa_ppx.import *)
[%%import: Types.t
][@@deriving quotation_test {
        test_types = [t]
  }]
