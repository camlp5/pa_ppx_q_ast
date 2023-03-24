(**pp -syntax camlp5o -package camlp5,compiler-libs.common *)

type t = A | B

type 'a loc = { txt : 'a ; loc : Location.t }
type t2 = { f1 : string loc }

