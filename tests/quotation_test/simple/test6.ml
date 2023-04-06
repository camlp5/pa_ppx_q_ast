(**pp -syntax camlp5o $(IMPORT_OCAMLCFLAGS) -package pa_ppx.import *)
[%%import: Types.t6
 [@add type t4 = [%import: Types.t4]]
][@@deriving quotation_test {
        test_types = [t6]
      ; expand_types = {
          t4 = Auto
        }
  }]
