lingl
=====

This is a language with a type system that enforces the correctness of linear algebra operations.
There is a compiler, pretty printer, and interpreter.


Set Up
------

We need [Dune][], [Jbuilder][], and [Menhir][]:

    $ opam install jbuilder
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

    # make run src='example_directory_name'

For example: 

    # make run src=lighting

You can run the compiler against an lgl file by executing 'exec' with a single additional argument
For example:

    $  jbuilder exec bin/lingc.bc 'filename'

To also print interpreter output, include an additional argument v

    $  jbuilder exec bin/lingc.bc 'filename' v

Examples
---

To see examples of shaders in action, simply pipe compiler output into the examples/color.json

    $ jbuilder exec bin/lingc.bc 'filename' > color.json

And then run the appropriate typescript file (either examples/trivial or examples/lighting)

    $ cd examples/trivial; make view

See the Makefile in examples/trivial for details on setting up your typescript environment

Tests
-----

There are a bunch of tests under the `test` directory.
You can run them all by typing `./test/test.sh`.
(But it's not clear how to tell whether they passed.)

Versions
-----
OpenGL 1.0
