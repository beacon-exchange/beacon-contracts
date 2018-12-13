.PHONY: test clean all asm

sources := $(templates/*.sol src/*.hs stack.yaml package.yaml)
main := artifacts/sol/Exchange.sol

all: bins

clean:
	rm -r artifacts/*

codegen: $(sources)
	mkdir -p artifacts/sol
	stack build
	stack exec codegen

artifacts/Exchange.asm: codegen
	solc --asm --allow-paths . $(main) > artifacts/Exchange.asm

asm: artifacts/Exchange.asm
	cat artifacts/Exchange.asm

# TODO fix dep
artifacts/bin/Exchange.bin: codegen
	mkdir -p artifacts/bin
	solc --overwrite -o artifacts/bin/ --optimize --bin --allow-paths artifacts/sol/ $(main)

bins: artifacts/bin/Exchange.bin
