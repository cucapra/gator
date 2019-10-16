# Linguine Examples

This directory contains full examples that demonstrate WebGL shaders written in Linguine.
The examples currently consist of:

* **flat_color:** The simplest possible shader! Just draws an object with every pixel set to a constant color.
* **phong:** The [Phong lighting model][phong].

[phong]: https://en.wikipedia.org/wiki/Phong_reflection_model
[blinn-phong]: https://en.wikipedia.org/wiki/Blinn%E2%80%93Phong_shading_model

To get setup, first install [npm][] and [yarn][].  Then run `npm install` and `yarn install` in this folder.

[npm]: https://www.npmjs.com/get-npm
[yarn]: https://yarnpkg.com/lang/en/docs/install/

To run an example, type:

    $ SRC=flat_color yarn run start

but replace `flat_color` with the directory name for any example.
This will print out a URL you can open in a browser to view the output.

This works by invoking our `start` script in `package.json`, which uses the `$SRC` environment variable to build and run a given example using [Parcel][].
We have a Parcel plugin that compiles Linguine source code to GLSL and then allows our TypeScript host programs to import the result as a string.
This pipeline requires the `gatorc` compiler program to be available on your path, i.e., that you have run `dune install` already.

The `raw` directory contains example code we're still working on.
Eventually, all this code will migrate to individual complete examples and we will delete these old files.

[parcel]: https://parceljs.org
