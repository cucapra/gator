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


Run
---

You can run the compiler against an lgl file by running
For example:

    $  jbuilder exec bin/ex.bc 'filename'

To also print interpreter output, include an additional argument v

    $  jbuilder exec bin/ex.bc 'filename' v

Examples
---

To see examples of shaders in action, simply pipe compiler output into the examples/color.json

    $ jbuilder exec bin/ex.bc 'filename' > color.json

And then run the appropriate typescript file (either examples/trivial or examples/lighting)

    $ cd examples/trivial; make view

See the Makefile in examples/trivial for details on setting up your typescript environment

Tests
-----

There are a bunch of tests under the `test` directory.
You can run them all by typing `./test/test.sh`.
(But it's not clear how to tell whether they passed.)
