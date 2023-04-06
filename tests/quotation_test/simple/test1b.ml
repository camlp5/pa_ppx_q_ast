(**pp -syntax camlp5o *)
type t = Types.t = A | B[@@deriving quotation_test { test_types = [t] }]
