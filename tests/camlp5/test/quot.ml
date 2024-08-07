(**pp -syntax camlp5o -package pa_ppx.import *)
[%%import: MLast.expr
    [@add [%%import: MLast.loc]]
    [@add [%%import: MLast.type_var]]
][@@deriving quotation_test {
        target_is_pattern_ast = true
      ; location_type = [%typ: loc]
      ; test_types = [
        	longid
              ; ctyp
              ; poly_variant
              ; patt
              ; expr
              ; module_type
              ; sig_item
              ; with_constr
              ; module_expr
              ; str_item
              ; type_decl
              ; generic_constructor
              ; extension_constructor
              ; type_extension
              ; class_type
              ; class_sig_item
              ; class_expr
              ; class_str_item
              ; attribute_body
        ]
      ; expand_types = [
          ([%typ: attributes], Auto)
        ; ([%typ: payload], Auto)
        ; ([%typ: generic_constructor], Auto)
        ; ([%typ: extension_constructor], Auto)
        ; ([%typ: type_extension], Auto)
        ]
      ; superfluous_constructors = [
          CeXtr
        ; CtXtr
        ; StXtr
        ; SgXtr
        ; MeXtr
        ; MtXtr
        ; LiXtr
        ; TyXtr
        ; ExXtr
        ; PaXtr
        ]
      }
  ]
