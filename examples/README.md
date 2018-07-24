# Linguine Examples

This directory contains full examples that demonstrate WebGL shaders written in Linguine.
The examples currently consist of:

* **flat_color:** The simplest possible shader! Just draws an object with every pixel set to a constant color.
* **lighting:** The [Phong lighting model][phong].
* **texture:** Demonstrates texture mapping.

[phong]: https://en.wikipedia.org/wiki/Phong_reflection_model

To run an example, type:

    $ SRC=flat_color yarn run start

but replace `flat_color` with the directory name for any example.
(Our `start` script in `package.json` uses the `$SRC` environment variable to build and run a given example using [Parcel][].)
This will print out a URL you can open in a browser to view the output.

[parcel]: https://parceljs.org
