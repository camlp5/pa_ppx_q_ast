(**pp -syntax camlp5o -package camlp5,compiler-libs.common *)

type t = A | B

type 'a located = { txt : 'a ; loc : Location.t }
type t2 = { f1 : string located }

type t3a = U | V
and t3b = X of t3a | Y of t3a | Z of t3a located

type t4 = U of num
and num = int

type t4' = V of num

type t5 = A of t4 * t4
        | B of t4 * t4'

type t6 = W of t4 list Ploc.vala option

type t7 = A of t3a
and t7' = B of t3a

type t8 = C of t8 * int | D

type t9 = num located

type t10 = (loc * string Ploc.vala * (string list) Ploc.vala * (string option) Ploc.vala)
and loc = Location.t
and s = string

type t11 = { a : int ; b : (int * (bool * char)) list }

type t12 =
  A1 of t12b option *  t12c option
| A2 of t12b option *  t12c option
| A3 of t12b t12d
and t12b = B1  | B2
and t12c = C1  | C2
and 'a t12d = { a : 'a option }

module T13 = struct
type t13 = A of t13b Ploc.vala option Ploc.vala * string
and t13b = B1 | B2
end

module T14 = struct
type t14 = A of { f1 : string ; f2 : string }
end
