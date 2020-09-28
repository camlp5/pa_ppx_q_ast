A PPX Rewriter for Q_ast

### Version

This is ``pa_ppx_q_ast`` (alpha) version 0.01.

# Overview

Camlp5's `Q_ast` is a powerful tool.  It allows the user to write
parsers for some data-type (doesn't have to be OCaml's AST) and then,
after having written some "extra code", it automatically provides
quotations over that data-type.  Which is ... magical.  But the cost
is that "extra code", which can be pretty voluminous, and is always
boilerplate.

This PPX rewriter is an attempt to automate the generation of that
extra code.

# Usage
