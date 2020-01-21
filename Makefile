.PHONY: all build run

all: build

build:
	cabal new-build

run:
	cabal new-run
