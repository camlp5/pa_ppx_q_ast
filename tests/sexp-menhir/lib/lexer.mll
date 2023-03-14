(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* The lexer definition *)

{
open Lexing
open Misc
open Parser

type error =
  | Illegal_character of char

exception Error of error * Location.t

let error lexbuf e = raise (Error(e, Location.curr lexbuf))
let error_loc loc e = raise (Error(e, loc))

let update_loc lexbuf file line absolute chars =
  let pos = lexbuf.lex_curr_p in
  let new_file = match file with
                 | None -> pos.pos_fname
                 | Some s -> s
  in
  lexbuf.lex_curr_p <- { pos with
    pos_fname = new_file;
    pos_lnum = if absolute then line else pos.pos_lnum + line;
    pos_bol = pos.pos_cnum - chars;
  }

}

(* The table of keywords *)

let newline = ('\013'* '\010')
let blank = [' ' '\009' '\012']
let lowercase = ['a'-'z' '_']
let uppercase = ['A'-'Z']
let identchar = ['A'-'Z' 'a'-'z' '_' '\'' '0'-'9']

let ident = (lowercase | uppercase) identchar*

rule token = parse
  | newline
      { update_loc lexbuf None 1 false 0;
        token lexbuf }
  | blank +
      { token lexbuf }
  | ident* as atom
      { ATOM atom }
  | "("  { LPAREN }
  | ")"  { RPAREN }
  | "."  { DOT }
  | eof { EOF }
  | (_ as illegal_char)
      { error lexbuf (Illegal_character illegal_char) }


