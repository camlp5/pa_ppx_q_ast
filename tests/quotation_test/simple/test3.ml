(**pp -syntax camlp5o $(IMPORT_OCAMLCFLAGS) -package pa_ppx.import *)
[%%import: Type1.t2
 [@add
     [%%import: Type1.loc
      [@with Location.t := location]
     ]
  type location = Location.t
 ]
][@@deriving quotation_test {
        test_types = [t2]
      ; expand_types = [
          loc
        ]
      ; location_type = [%typ: location]
      ; loc_varname = __loc__
  }]
