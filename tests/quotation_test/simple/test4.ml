(**pp -syntax camlp5o $(IMPORT_OCAMLCFLAGS) -package pa_ppx.import *)
[%%import: Types.t3b
  [@add [%%import: Types.loc]]
][@@deriving quotation_test {
        test_types = [t3b]
      ; expand_types_per_constructor = [
          (X, { t3a = Auto })
        ; (Z, { t3a = Auto ; loc = Auto })
        ]
  }]
