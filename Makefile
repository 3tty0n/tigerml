
all: build test

build:
	dune build

clean:
	dune clean

test:
	./test.sh
