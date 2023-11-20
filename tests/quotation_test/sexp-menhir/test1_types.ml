(**pp -syntax camlp5o -package pa_ppx.import *)
[%%import: Sexp.Pattern.sexp
][@@deriving quotation_test {
        target_is_pattern_ast = false
      ; location_type = [%typ: Location.t]
      ; loc_varname = __loc__
      ; superfluous_constructors = [
          Xtra
        ]
      ; test_types = [
        	sexp
        ]
      ; type_module_map = {
          sexp = Sexp.Normal
        }
      }
  ]
