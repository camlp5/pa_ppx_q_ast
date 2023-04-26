(**pp -syntax camlp5o $(IMPORT_OCAMLCFLAGS) -package pa_ppx.import *)
[%%import: Types.t9
 [@add type num = [%import: Types.num]]
 [@add type 'a located = [%import: 'a Types.located]]
][@@deriving quotation_test {
        test_types = [t9]
      ; minimal_record_module_labels = true
      ; expand_types = {
          located = Auto
        ; num = Explicit[0;1]
        }
  }]
