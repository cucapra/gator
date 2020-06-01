BUILD_ARGS=$(if (command -v ocamlformat),@install @fmt --auto-promote,@install)

.PHONY: all clean repl run build test

all:
	dune exec bin/gatorc.ml

repl:
	dune utop src

examples/%:
	python3 scripts/jsonify.py $@

run:
	cd examples/; SRC=$(src) yarn run start

build:
	dune build ${BUILD_ARGS} bin/gatorc.ml
	dune build ${BUILD_ARGS} && dune install

clean:
	dune clean
	rm test/**/*.out || true
	# rm test-u/**/*.out || true

test:
	python3 test.py