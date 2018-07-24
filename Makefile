.PHONY: all clean repl run

all:
	dune exec bin/lingc.bc

repl:
	dune utop src

examples/%:
	python3 scripts/jsonify.py $@

run:
	make -C examples/$(src)
	cd examples/; SRC=$(src) yarn run start

clean:
	dune clean
