lingl
=====

This is a language with a type system that enforces the correctness of linear algebra operations.
There is a parser, a pretty printer, and an interpreter.


Set Up
------

We need [Dune][] and [Menhir][]:

    $ opam install jbuilder
    $ opam install menhir

Build by typing:

    $ jbuilder build bin/ex.bc

Explore the implementation in a [utop][] REPL:

    $ opam install utop
    $ jbuilder utop src

[dune]: https://github.com/ocaml/dune
[menhir]: http://gallium.inria.fr/~fpottier/menhir/
[utop]: https://github.com/diml/utop
