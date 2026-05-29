(**pp -syntax camlp5o -package pa_ppx.import *)
[%%typedecls
[%%import: Types.t]
[%%import: Types.t6b]
]
[@@deriving quotation_test {
        target_is_pattern_ast = true
      ; test_types = [t6b]
      ; expand_types = [
          ([%typ: t], Auto)
        ]
  }]

[@@@ocaml.text "not-pattern"]
[%%typedecls
[%%import: Types.t]
[%%import: Types.t6b]
]
[@@deriving quotation_test {
        target_is_pattern_ast = false
      ; test_types = [t6b]
      ; expand_types = [
          ([%typ: t], Auto)
        ]
  }]
