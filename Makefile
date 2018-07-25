.PHONY: all clean repl run build

all:
	dune exec bin/lingc.bc

repl:
	dune utop src

examples/%:
	python3 scripts/jsonify.py $@

run:
	make -C examples/$(src)
	cd examples/; SRC=$(src) yarn run start

build: 
	dune build bin/lingc.bc
	dune build && dune install

clean:
	dune clean
