
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

let vaval x = Ploc.VaVal x

%}

/* Tokens */

%token LPAREN "("
%token RPAREN ")"
%token DOT "."
%token <string> ATOM        "atom" (* just an example *)
%token EOF                    ""
%token <string * Location.t> ANTI
%token <string * Location.t> ANTI_ATOM

/* Entry points */


%type <Sexp.Normal.sexp> parse_sexp
%start parse_sexp

%type <Sexp.Pattern.sexp> parse_pattern_sexp
%start parse_pattern_sexp

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

%inline vaval(X):
   X
   { vaval $1 }
;

%inline vala(X,anti):
   X
     { vaval $1 }
  | anti
     { Ploc.VaAnt (fst $1) }
;

(* Entry points. *)

parse_sexp:
  sexp EOF
    { $1 }
;

parse_pattern_sexp:
  pattern_sexp EOF
    { $1 }
;

(* -------------------------------------------------------------------------- *)

sexp: LPAREN RPAREN
      { Normal.Nil (make_loc $sloc) }
| LPAREN l=sexp_list RPAREN
      { l }
| a=ATOM
      { Normal.Atom (make_loc $sloc, a) }
;

sexp_list:
  l=sexp
    { Normal.Cons (make_loc $sloc, l, Normal.Nil (make_loc $sloc)) }
| l=sexp r=sexp_list
    { Normal.Cons (make_loc $sloc, l, r) }
| l=sexp DOT r=sexp
    { Normal.Cons (make_loc $sloc, l, r) }
;

pattern_sexp: LPAREN RPAREN
      { Pattern.Nil (make_loc $sloc) }
| LPAREN l=pattern_sexp_list RPAREN
      { l }
| a=vala(ATOM, ANTI_ATOM)
      { Pattern.Atom (make_loc $sloc, a) }
;

pattern_sexp_list:
  l=pattern_sexp
    { Pattern.Cons (make_loc $sloc, l, Pattern.Nil (make_loc $sloc)) }
| l=pattern_sexp r=pattern_sexp_list
    { Pattern.Cons (make_loc $sloc, l, r) }
| l=pattern_sexp DOT r=pattern_sexp
    { Pattern.Cons (make_loc $sloc, l, r) }
| a=ANTI
    { Pattern.Xtra (snd a, fst a) }
;

%%
