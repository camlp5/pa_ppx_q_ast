(**pp -syntax camlp5o -package pa_ppx.import *)
[%%import: Types.t3b
  [@add [%%import: Types.located]]
][@@deriving quotation_test {
        test_types = [t3b]
      ; minimal_record_module_labels = true
      ; expand_types_per_constructor = [
          (X, { t3a = Auto })
        ; (Z, { t3a = Auto ; located = Auto })
        ]
  }]
