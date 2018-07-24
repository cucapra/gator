.PHONY: all clean repl run

all:
	dune exec bin/lingc.bc

repl:
	dune utop src

examples/%:
	python3 scripts/jsonify.py $@

run:
	python3 scripts/jsonify.py examples/$(src);
	cd examples/; SRC=$(src) npm start

clean:
	dune clean
