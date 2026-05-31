(**pp -syntax camlp5o -package pa_ppx.import,pa_ppx_q_ast_quotation_test.full *)
[%%typedecls
[%%import: Types.t16]
[%%import: Types.t]
[%%import: Types.t4]
]
[@@deriving quotation_test {
        test_types = [t16]
      ; expand_types = [
          ([%typ: t], Auto)
        ; ([%typ: t4], Auto)
        ; ([%typ: t16],

           DelPatts([
                 [%patt? Types.T16 (Types.U x00, Types.A, Some x01)]
               ; [%patt? Types.T16 (Types.U x00, Types.A, x01)]
             ])
          )
        ]
      ; minimal_record_module_labels = true
  }]
