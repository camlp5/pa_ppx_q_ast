(**pp -syntax camlp5o -package pa_ppx.import,pa_ppx_q_ast_quotation_test.full *)
[%%typedecls
[%%import: Types.t17]
[%%import: Types.t]
]
[@@deriving quotation_test {
        test_types = [t17]
      ; expand_types = [
          ([%typ: t], Auto)
        ; ([%typ: t17],
           DelPatts([
                 [%patt? {Types.a = Types.A}]
               ; [%patt? {d = Types.B}]
             ])
          )
        ]
      ; minimal_record_module_labels = true
  }]
