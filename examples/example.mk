# Include this from an example's Makefile to quickly get a target for
# compiling Linguine source code to GLSL, bundled into JSON.

# Specify SRC to customize the list of Linguine files to process.
SRC ?= $(wildcard *.lgl)
GLSL := $(SRC:%.lgl=%.glsl)

# The directory where this file resides.
HERE := $(dir $(lastword $(MAKEFILE_LIST)))

# The Linguine compiler executable. This could be
# `dune exec ../../bin/lingc.bc` or similar, for example, to use a
# non-installed version.
LINGC := lingc

# The "jsonify" tool, which bundles up the contents of individual files into a
# single JSON object.
JSONIFY := $(HERE)/../scripts/jsonify.py

data.json: $(GLSL)
	$(JSONIFY) $^ > $@

# Compile Linguine programs.
%.glsl: %.lgl
	$(LINGC) $^ > $@
