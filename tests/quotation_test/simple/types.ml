(**pp -syntax camlp5o -package camlp5,compiler-libs.common *)

type t = A | B

type 'a loc = { txt : 'a ; loc : Location.t }
type t2 = { f1 : string loc }

type t3a = U | V
and t3b = X of t3a | Y of t3a | Z of t3a loc

type t4 = U of num
and num = int

type t4' = V of num

type t5 = A of t4 * t4
        | B of t4 * t4'
