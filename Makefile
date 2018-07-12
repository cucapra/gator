.PHONY: all clean

all:
	dune build bin/ex.bc

clean:
	dune clean
