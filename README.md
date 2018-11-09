lingl
=====

[![CircleCI](https://circleci.com/gh/cucapra/linguine.svg?style=svg)](https://circleci.com/gh/cucapra/linguine)


This is a language with a type system that enforces the correctness of linear algebra operations.
There is a compiler, pretty printer, and interpreter.

OCaml 4.04 or higher required


Set Up
------

We need [Dune][] and [Menhir][]:

    $ opam install dune
    $ apt-get install m4  # On Debian, for example.
    $ opam install menhir

Build by typing:

    $ dune build bin/lingc.bc

Now you can use `dune exec bin/lingc.bc` to run the compiler.
Or you can install a `lingc` executable:

    $ dune build && dune install

Now `lingc` should be available on your path.

[dune]: https://github.com/ocaml/dune
[menhir]: http://gallium.inria.fr/~fpottier/menhir/

Run
---

To simulate a Linguine-compiled shader on your browser:

    $ make run src='example_directory_name'

For example:

    $ make run src=phong

You can run the compiler by passing the `*.lgl` source file as an argument to `lingc`.
For example:

    $ lingc example.lgl

To also print interpreter output, include an additional argument v

    $ lingc example.lgl v

Tests
-----

There are a bunch of tests under the `test` directory.
Use the `test.py` script to run them.

Technical Details
-----------------

The current version of the compiler generates GLSL 1.0 code, which is suitable for use in WebGL 1.0 (i.e., OpenGL 2.0) programs.
