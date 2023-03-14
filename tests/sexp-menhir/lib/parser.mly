
/* The parser definition */

%{

open Sexp

let mkloc = Location.mkloc
let mknoloc = Location.mknoloc

let make_loc (startpos, endpos) = {
  Location.loc_start = startpos;
  Location.loc_end = endpos;
  Location.loc_ghost = false;
}

let ghost_loc (startpos, endpos) = {
  Location.loc_start = startpos;
  Location.loc_end = endpos;
  Location.loc_ghost = true;
}

%}

/* Tokens */

%token LPAREN "("
%token RPAREN ")"
%token DOT "."
%token <string> ATOM        "atom" (* just an example *)
%token EOF                    ""

/* Entry points */


%type <Sexp.sexp> parse_sexp
%start parse_sexp

%%

/* Generic definitions */

%inline mkloc(symb): symb
    { mkloc $1 (make_loc $sloc) }

(* [llist(X)] recognizes a possibly empty list of [X]s. It is left-recursive. *)

reversed_llist(X):
  /* empty */
    { [] }
| xs = reversed_llist(X) x = X
    { x :: xs }

%inline llist(X):
  xs = rev(reversed_llist(X))
    { xs }

(* [reversed_nonempty_llist(X)] recognizes a nonempty list of [X]s, and produces
   an OCaml list in reverse order -- that is, the last element in the input text
   appears first in this list. Its definition is left-recursive. *)

reversed_nonempty_llist(X):
  x = X
    { [ x ] }
| xs = reversed_nonempty_llist(X) x = X
    { x :: xs }

(* [nonempty_llist(X)] recognizes a nonempty list of [X]s, and produces an OCaml
   list in direct order -- that is, the first element in the input text appears
   first in this list. *)

%inline nonempty_llist(X):
  xs = rev(reversed_nonempty_llist(X))
    { xs }

(* Entry points. *)

parse_sexp:
  sexp EOF
    { $1 }
;

(* -------------------------------------------------------------------------- *)

sexp: LPAREN RPAREN
        { Nil (make_loc $sloc) }
;

%%
