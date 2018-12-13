.PHONY: test clean all asm

sources := $(templates/*.sol src/*.hs stack.yaml package.yaml)
main := src/Main.sol

all: bins

clean:
	rm -r artifacts/*

asm:
	solc --asm --allow-paths . $(main)

codegen: $(sources)
	mkdir -p artifacts/sol
	stack build
	stack exec codegen

# TODO fix dep
artifacts/bin/Exchange.bin: codegen
	mkdir -p artifacts/bin
	solc --overwrite -o artifacts/bin/ --optimize --bin --allow-paths artifacts/sol/ artifacts/sol/Exchange.sol

bins: artifacts/bin/Exchange.bin
