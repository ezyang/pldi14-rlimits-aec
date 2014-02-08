GHC=/home/hs01/ezyang/ghc-alloc/inplace/bin/ghc-stage2
FAST=1

.PHONY: init tests build-deps

damp: Main.hs
	${GHC} --make Main.hs -package ghc -o damp -rtsopts -fforce-recomp

init: damp
	./script.sh init

tests: damp
	./script.sh runtests ${FAST}

data.csv: init tests
	./generate-data.sh data.csv

data.pdf: data.csv
	R --no-save < do.R

build-deps:
	cabal install --with-ghc=${GHC} ghc-paths
