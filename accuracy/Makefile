GHC=/home/hs01/ezyang/ghc-alloc/inplace/bin/ghc-stage2
FAST=1

damp: Main.hs
	${GHC} --make Main.hs -package ghc -o damp -rtsopts -fforce-recomp

init:
	./script.sh init

tests: init
	./script.sh runtests ${FAST}

data.csv: tests
	./generate-data.sh data.csv

data.pdf: data.csv
	R --no-save < do.R

build-deps:
	cabal install --with-ghc=${GHC} ghc-paths
