(**pp -syntax camlp5o $(IMPORT_OCAMLCFLAGS) -package pa_ppx.import *)
[%%import: Types.t9
 [@add type num = [%import: Types.num]]
 [@add type 'a loc = [%import: 'a Types.loc]]
][@@deriving quotation_test {
        test_types = [t9]
      ; expand_types = {
          loc = Auto
        ; num = Explicit[0;1]
        }
  }]
