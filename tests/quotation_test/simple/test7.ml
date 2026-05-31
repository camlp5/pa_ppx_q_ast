(**pp -syntax camlp5o -package pa_ppx.import,pa_ppx_q_ast_quotation_test,pa_ppx_q_ast_test_cleanup,pa_ppx_q_ast_test_renumber *)
[%%import: Types.t7
 [@add type t3a = [%import: Types.t3a]]
][@@deriving quotation_test {
        test_types = [t7; t7']
      ; expand_types_per_type = {
          t7' = [
            ([%typ: t3a], Auto)
          ]
        ; t7 = [
            ([%typ: t7], Explicit [Types.A U])
          ]
        }
  }]
