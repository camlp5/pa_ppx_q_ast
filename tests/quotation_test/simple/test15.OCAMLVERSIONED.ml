(**pp -syntax camlp5o -package pa_ppx.import *)
#if OCAML_VERSION >= (5,4,0)
[%%import: Types.T15.t15
]
[@@deriving quotation_test {
        test_types = [t15]
      ; minimal_record_module_labels = true
      ; type_module_map = {
          t15 = T15
        }
  }]
#else
type t15 = unit
[@@deriving quotation_test {
        test_types = [t15]
      ; minimal_record_module_labels = true
  }]
#endif
