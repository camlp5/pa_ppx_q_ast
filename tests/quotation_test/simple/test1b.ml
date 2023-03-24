(**pp -syntax camlp5o *)
type t = Type1.t = A | B[@@deriving quotation_test { test_types = [t] }]
