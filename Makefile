.PHONY: all clean repl run

all:
	dune exec bin/ex.bc

repl:
	dune utop src

examples/%:
	[ -d $@ ] || mkdir $@
	python3 scripts/jsonify.py $@ $* > $@/$*.json

# init: 

run:
	cd examples/$(src); $(MAKE) view

clean:
	jbuilder clean