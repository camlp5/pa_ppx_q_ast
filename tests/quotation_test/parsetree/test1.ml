(**pp -syntax camlp5o $(IMPORT_OCAMLCFLAGS) -package pa_ppx.import *)
[%%import: Parsetree.attribute
  [@add
      [%%import: Lexing.position]
      type location = [%import: Location.t
                                    [@with Lexing.position := position]
                        ]
      type 'a located = [%import: 'a Asttypes.loc
                                    [@with Location.t := location]
                        ]
      type longident_t = [%import: Longident.t
                                     [@with Lexing.position := position]
                                     [@with t := longident_t]
                         ]
      type ast_constant =
        [%import: Asttypes.constant
                    [@with Location.t := location]
        ]
      [%%import: Asttypes.arg_label]
      [%%import: Asttypes.label]
      [%%import: Asttypes.closed_flag]
      [%%import: Asttypes.rec_flag]
      [%%import: Asttypes.direction_flag]
      [%%import: Asttypes.private_flag]
      [%%import: Asttypes.mutable_flag]
      [%%import: Asttypes.virtual_flag]
      [%%import: Asttypes.override_flag]
      [%%import: Asttypes.variance]
      [%%import: Asttypes.injectivity]
      [%%import: Parsetree.constant
                   [@with Location.t := location]
      ]
      [%%import: Parsetree.location_stack
                   [@with Location.t := location]
      ]
      [%%import: Parsetree.toplevel_phrase
                   [@with Location.t := location]
                   [@with Asttypes.loc := located]
                   [@with Longident.t := longident_t]
            ]
  ]
 [@with Location.t := location]
 [@with Asttypes.loc := located]
 [@with Longident.t := longident_t]
 [@with Asttypes.arg_label := arg_label]
 [@with Asttypes.label := label]
 [@with Asttypes.closed_flag := closed_flag]
 [@with Asttypes.rec_flag := rec_flag]
 [@with Asttypes.direction_flag := direction_flag]
 [@with Asttypes.private_flag := private_flag]
 [@with Asttypes.mutable_flag := mutable_flag]
 [@with Asttypes.virtual_flag := virtual_flag]
 [@with Asttypes.override_flag := override_flag]
 [@with Asttypes.variance := variance]
 [@with Asttypes.injectivity := injectivity]
][@@deriving quotation_test {
        location_type = [%typ: location]
      ; loc_varname = __loc__
      ; test_types = [
        	expression
        ]
      ; default_expression = {
          location_stack = []
        }
      ; expand_types = [
          expression_desc
        ; attributes
        ; payload
        ; generic_constructor
        ; extension_constructor
        ; type_extension
        ]
      }
  ]
