lingl
================================

Credit README skeleton to Adrian

This is a simple parser and interpreter implementation for checking the
  tag-correctness of matrix operations.
This implementation parses, pretty-prints, and evaluates a minimal AST

We need [Dune][] and [Menhir][]:

    $ opam install jbuilder
    $ opam install menhir

Build by typing:

    $ jbuilder build bin/ex.bc

Run an example:

    $ echo '8*(3+2)+2' | jbuilder exec bin/ex.bc

Explore the implementation in a [utop][] REPL:

    $ opam install utop
    $ jbuilder utop src

This repository contains:

* `src/`: The language implementation.
    * `ast.ml`: An ADT for the language syntax.
    * `parser.mly`: A [Menhir][] parser for expressions.
    * `lexer.mll`: The [ocamllex][lexyacc] lexer for that parser.
    * `ops.ml`: Functions to evaluate expressions.
    * `matrix_ops.ml`: Functions to interpret matrix operations
    * `print.ml`: Functions to pretty-print expressions.
* `bin/ex.ml`: A tool that reads a program from standard input.
* `src/jbuild` and `bin/jbuild`: The build configuration for [Dune][].

[dune]: https://github.com/ocaml/dune
[menhir]: http://gallium.inria.fr/~fpottier/menhir/
[lexyacc]: https://caml.inria.fr/pub/docs/manual-ocaml/lexyacc.html
[utop]: https://github.com/diml/utop
