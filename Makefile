.PHONY: all clean repl run

all:
	dune exec bin/ex.bc

repl:
	dune utop src

examples/%:
	python3 scripts/jsonify.py $@ $*

init: 
	[ -d $@ ] || mkdir $@
	mv $@_f.lgl $@/
	mv $@_v.lgl $@/
	python3 scripts/init.py $@ $*

run:
	cd examples/$(src); $(MAKE) view

clean:
	dune clean