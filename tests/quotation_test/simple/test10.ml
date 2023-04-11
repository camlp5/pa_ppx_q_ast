(**pp -syntax camlp5o $(IMPORT_OCAMLCFLAGS) -package pa_ppx.import *)
type t10 = (loc * string Ploc.vala * (string list) Ploc.vala * (string option) Ploc.vala)
and loc = Location.t
and s = string
(*
[%%import: Types.t10
]
 *)
[@@deriving quotation_test {
        test_types = [t10]
  }]
