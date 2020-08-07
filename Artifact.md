This repository contains the compiler, examples, and evaluation materials for the OOPSLA 2020 paper [Geometry Types for Graphics Programming](https://www.cs.cornell.edu/~dgeisler/oopsla2020.pdf).

This artifact has 3 components:
* The Gator compiler and test suite
* A collection of graphics examples written in the Gator language with the infrastructure to run them in a web browser
* A script for gathering timing information about these graphics examples.

## Prerequisites

There are two main ways to access this artifact:

* The included virtual machine image
* Building this repository yourself

With the virtual machine image, you will just need a hypervisor capable of running the image.  Using the VM will produce results that work, but will be very slow since modern VMs will likely use software for rendering rather than the GPU.  Otherwise, to build this repository, you will need to clone the `oopsla2020` branch of this repository, and follow the setup instructions in `README.md` of the root directory, the setup instructions in `examples/README.md`, _and_ the setup instructions in `benchmarks/README.md`.

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
  - (_optional_) examine statistics script
  - Run example shaders to view realtime images
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

Next, execute `cd examples` to navigate to the examples folder.  In this folder, run the following command to start a server to display a sample shader (use Ctrl-C to terminate the server):
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

Navigate to `linguine/benchmarks`.  If not using the VM, you will need to follow the installation instructions for the python3 libraries (the Chromium installation is not required until a later step).  To regenerate the graph used in the paper, run the command:
```
python3 visualize.py run_oopsla.json
```
This script will output the graph used in the paper, as well as the data used in Table 1.  The calculations for the statistics included in the paper are all done in the script, so it can be examined if there are questions about our methodologies.

Next, to generate the sample images used in Figure 7 of the paper, navigate to `linguine/examples`.  The images can be created on a server using the following commands in sequence, navigating to `localhost:1234` to view each associated image (and Ctrl-C to terminate the server):
```
SRC=texture_obj/ yarn run start
SRC=reflection/ yarn run start
SRC=shadow_map/ yarn run start
SRC=microfacet/ yarn run start
```
When viewing the image, you may use the mouse buttons and wheel to move the camera to capture the angle used for each image in the paper.

The shader code associated with each of these examples can be seen in the `.lgl` files 

## Run Framerate Experiments (~30 minutes)

We expect that running these experiments on a virtual machine will result in _much_ lower framerates for all experiments; running on a virtual machine likely uses software rendering rather than the GPU to render images, which results in choppier graphics and much slower framerates.  This can only be avoided by running the experiments on a non-virtual machine.  

We will simulate running the experiments used to generate the data used when generating the graph in Figure 8 and the table in Table 1.  This experiment works by launching a browser and remote-controlling it to simulate the web shaders in a real environemnt.

To run this experiment, navigate to `linguine/benchmarks` and run the command:
```
python3 main.py
```
This script will open a chrome window with the appropriate test case automatically, and start recording framerate.  While running these experiments, we made sure to keep the window running in the foreground and to not run background processes to help ensure the framerate would not be affected by external processes.

Results are written both to `data/run.json` and `data/[current_date]_run.json`. After running the experiments, you can run `visualize.py data/run.json` (or whichever file you wish to view) to see the table of results and plot of data.

For this experiment, we shortened the length by a factor of 10 by reducing the number of benchmarks in `main.py` from `* 30` to `* 3`; this should make running the benchmarks more reasonable for the reviewer; however, we expect that the results will have a much higher error than in our paper results.  This change can be easily reverted if the reviewer would like to minimize error by changing line 67 of `main.py`.