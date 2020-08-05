Gator
=====

[![CircleCI](https://circleci.com/gh/cucapra/linguine.svg?style=svg)](https://circleci.com/gh/cucapra/linguine)


This is a flexible language built around geometric types for tracking the geometry of a run program.
There is a compiler, pretty printer, and interpreter.

OCaml 4.04 or higher required


Set Up
------

We need [Dune][] and [Menhir][]:

    $ opam install dune
    $ apt-get install m4  # On Debian, for example.
    $ opam install menhir

Build by typing:

    $ make

Now you can use `dune exec bin/gatorc.bc` to run the compiler.
Or you can install a `gatorc` executable:

    $ make build

Now `gatorc` should be available on your path.

[dune]: https://github.com/ocaml/dune
[menhir]: http://gallium.inria.fr/~fpottier/menhir/

Tests
-----

There are a bunch of tests under the `test` directory.
Use the `test.py` script to run them and check that the compiler is working.

Run
---

To simulate a Gator-compiled shader on your browser, view the README in the `example` folder.

You can run the compiler directly by passing a `*.lgl` source file as an argument to `gatorc`.
For example:

    $ gatorc example.lgl

Technical Details
-----------------

The current version of the compiler generates GLSL 1.0 code, which is suitable for use in WebGL 1.0 (i.e., OpenGL 2.0) programs.
