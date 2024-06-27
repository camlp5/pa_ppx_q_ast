A PPX Rewriter for Q_ast

### Version

This is ``pa_ppx_q_ast`` (alpha) version 0.12.

# Overview

Camlp5's `Q_ast` is a powerful tool.  Once a user has specified a
data-type, and implemented a Camlp5 parser for that type (doesn't have
to be OCaml's AST), then with some "extra code", it automatically
provides quotations over that data-type.  Which is ... magical.  But
the cost is that "extra code", which can be pretty voluminous, and is
always boilerplate.

This PPX rewriter is an attempt to automate the generation of that
extra code.

# Usage
