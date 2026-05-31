(**pp -syntax camlp5o -package pa_ppx.import,pa_ppx_q_ast_quotation_test.full *)
[%%import: Types.t8
][@@deriving quotation_test {
        test_types = [t8]
  }]
