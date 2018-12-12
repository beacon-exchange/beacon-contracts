.PHONY: test clean all asm

sources := $(templates/*.sol src/*.hs stack.yaml package.yaml)
main := src/Main.sol

all: bins

clean:
	rm -r bin/*

asm:
	solc --asm --allow-paths . $(main)

codegen: $(patsubst templates/%.sol,artifacts/sol/%.sol, templates/*.sol)
	mkdir -p artifacts/sol
	stack build
	stack exec codegen

# TODO fix dep
bin/Main.bin: codegen
	mkdir -p artifacts/bin
	solc --overwrite -o artifacts/bin/ --optimize --bin --allow-paths artifacts/sol/ artifacts/sol/Main.sol

bins: bin/Main.bin
