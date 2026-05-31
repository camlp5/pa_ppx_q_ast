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
                 [%patt? Types.T16((Types.U _), Types.A)]
             ])
          )
        ]
      ; minimal_record_module_labels = true
  }]
