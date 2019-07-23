.PHONY: all clean repl run build

all:
	dune exec bin/gatorc.bc

repl:
	dune utop src

examples/%:
	python3 scripts/jsonify.py $@

run:
	cd examples/; SRC=$(src) yarn run start

build:
	dune build bin/gatorc.bc
	dune build && dune install

clean:
	dune clean
