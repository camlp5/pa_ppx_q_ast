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
