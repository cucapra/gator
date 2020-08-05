This repository contains the compiler, examples, and evaluation materials for the OOPSLA 2020 paper [Geometry Types for Graphics Programming](https://www.cs.cornell.edu/~dgeisler/oopsla2020.pdf).

This artifact has 3 components:
* The Gator compiler and test suite
* A collection of graphics examples
* A script for gathering timing information about these graphics examples.

## Prerequisites

There are two main ways to access this artifact:

* The included virtual machine image
* Building this repository yourself

With the virtual machine image, you will just need a hypervisor capable of running the image.  Otherwise, to build this repository, you will need to clone the `oopsla2020` branch of this repository, and follow the setup instructions in `README.md` of the root directory, the setup instructions in `examples/README.md`, _and_ the setup instrucitons in `benchmarks/README.md`.  These instructions require installation of the following tools:

* OCaml (version 4.04 or higher)
* Dune
* Menhir
* NPM
* Yarn
* Python 3 + pip + various libraries
* Chromium

## Virtual Machine Overview

The username and password are both `gator`.

The repository containing this document is located in `~/linguine`.  To get started, type `cd ~/linguine` and run `git pull` to update to the latest version of the `oopsla2020` branch of this repository.  We use this branch to simulate the compiler and examples in the state they were in when the paper was submitted, along with small updates to the README and this document.  Then run `make build` to build the latest version of the compiler and install `gatorc`.

## Step-by-step guide

For artifact evaluation, we would like the reviewers to go through the following steps, each of which is described in detail in a section below:

* Test Compiler Functionality
  - Run tests
  - Run example shaders
  - (_optional_) Examine test files
  - (_optional_) Examine standard library and shaders
  - (_optional_) Write a custom shader
* Regenerate Graphs and Sample Images from Paper
  - Run script on existing data to view graph
  - Run example shaders to view realtime images
  - (_optional_) examine graph data
  - (_optional_) examine relevant shader code
* Run Framerate Experiment
  - Run each shader for set amount of time in random order
  - (_optional_) Generate graph illustrating results

## Test Compiler Functionality (5-10 minutes)

Our goal in this section is to check that the compiler is functioning and to examine how it works in practice. 

In the root folder of our repository (`~/linguine` in the virtual machine), run the following command to run our test suite:
```
python test.py
```
If any tests fail, try running `make build` again to ensure the latest version of the compiler is built.

Next, execute `cd examples` to navigate to the examples folder.  In this folder, run the following command to start a server to display a sample shader:
```
SRC=auto_phong/ yarn run start
```
To view the shader, open a web browser and load the site `localhost:1234` as the URL.  You should see an alligator model with a light rotating around it.

Most shaders in the example folder can be run similarly by replacing `auto_phong` with the appropriate folder name and viewed at `localhost:1234`.  Some shaders are a work-in-progress, and so will not run with the current branch distribution.

### Optional Steps

Our tests are located in various folders in `linguine/test`.  Each test is a Gator file and is tested for compilability; the `out` files are currently meaningless, but will be used to check for correct output once a planned JavaScript interpreter is wired up.  We recommend examining tests in the `basics` folder to get a sense of the baseline capabilities of Gator, then looking at `types/geops.lgl` to get a sense of the generic nature and depth of operations supported by Gator.

All example shaders included with Gator rely on the standard library located in `examples/glsl_defs.lgl`.  This library contains definitions for a subset of GLSL 1.0 and a selection of coordinate schemes useful when defining standard geometric interactions.  The actual Gator code for each shader is contained in the `.lgl` file in each folder; we recommend examining `auto_phong/fragment.lgl` to get a sense of what Gator code should look like.

When starting with a custom shader in Gator, we recommend starting by examining existing shaders to get a sense of syntax and changing some basic values.  If you have a background in shader development, we would love it if you would try and write a custom shader using the Gator standard library.  Some GLSL functions may not be included yet, but similar syntax to existing functions can be used to include new functions in the standard library.

## Regenerate Graphs and Sample Images (10-20 minutes)

In this section, we will examine how to generate the graph in Figure 8, the data for the table in Table 1, and the images for Figure 7.

Navigate to `linguine/benchmarks`.  If not using the VM, you will need to follow the installation instructions for the python3 libraries and Chromium.