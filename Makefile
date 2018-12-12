.PHONY: test clean all

sources := $(lib/*.sol src/*.sol)
main := src/Main.sol

all: bins

clean:
	rm -r bin/*

asm: $(sources)
	solc --asm --allow-paths . $(main)

bin/Main.bin: $(sources)
	mkdir -p bin
	solc -o bin/ --optimize --bin --allow-paths . $(main)

bins: bin/Main.bin
