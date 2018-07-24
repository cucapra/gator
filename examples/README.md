# Linguine Examples

This directory contains full examples that demonstrate WebGL shaders written in Linguine.
The examples currently consist of:

* **flat_color:** The simplest possible shader! Just draws an object with every pixel set to a constant color.
* **lighting:** The [Phong lighting model][phong].
* **texture:** Demonstrates texture mapping.

[phong]: https://en.wikipedia.org/wiki/Phong_reflection_model

To run an example, type:

    $ make -C flat_color
    $ SRC=flat_color yarn run start

but replace `flat_color` with the directory name for any example.
This will print out a URL you can open in a browser to view the output.

Here's what's happens when you run those:

1. Each example has a `Makefile` that compiles the Linguine source code and combines it into a JSON file ready for execution in a browser.
2. Our `start` script in `package.json` uses the `$SRC` environment variable to build and run a given example using [Parcel][].

The `raw` directory contains example code we're still working on.
Eventually, all this code will migrate to individual complete examples and we will delete these old files.

[parcel]: https://parceljs.org
