(**pp -syntax camlp5o $(IMPORT_OCAMLCFLAGS) -package pa_ppx.import *)
[%%import: Types.t2
 [@add
     [%%import: Types.located
      [@with Location.t := location]
     ]
  type location = Location.t
 ]
][@@deriving quotation_test {
        test_types = [t2]
      ; minimal_record_module_labels = true
      ; expand_types = {
          located = Auto
        }
      ; location_type = [%typ: location]
      ; loc_varname = __loc__
  }]
