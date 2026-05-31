(**pp -syntax camlp5o -package pa_ppx.import,pa_ppx_q_ast_quotation_test,pa_ppx_q_ast_test_cleanup,pa_ppx_q_ast_test_renumber *)
[%%import: Types.t12
]
[@@deriving quotation_test {
        test_types = [t12]
      ; minimal_record_module_labels = true
      ; expand_types = [
          ([%typ: t12b], AddDel (
                     [],
                     [Types.B1]
                   ))
        ; ([%typ: t12d], Auto)
        ]
      ; expand_types_per_constructor = [
          (A2, [ ([%typ: t12c], AddDel (
                                    [],
                                    [Types.C1]
                 ))

               ; ([%typ: t12b option], AddDel (
                                    [],
                                    [None]
                 ))

          ])
        ]
  }]
