(**pp -syntax camlp5o $(IMPORT_OCAMLCFLAGS) -package pa_ppx.import *)
[%%import: Sexp.Pattern.sexp
][@@deriving quotation_test {
        location_type = [%typ: Location.t]
      ; test_types = [
        	sexp
        ]
      }
  ]
