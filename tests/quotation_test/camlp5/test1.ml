(**pp -syntax camlp5o $(IMPORT_OCAMLCFLAGS) -package pa_ppx.import *)
[%%import: MLast.expr
    [@add [%%import: MLast.loc]]
    [@add [%%import: MLast.type_var]]
][@@deriving quotation_test {
        ignore_types = [
        	class_infos
              ; case_branch
              ; longid_lident
              ; payload
              ; attribute_body
              ; type_var
        ]
      ; expand_types = [
          attributes
        ; payload
        ; generic_constructor
        ; extension_constructor
        ; type_extension
        ]
      }
  ]
