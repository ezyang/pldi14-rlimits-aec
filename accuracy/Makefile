GHC=/home/hs01/ezyang/ghc-alloc/inplace/bin/ghc-stage2

damp:
	${GHC} --make Main.hs -package ghc -o damp -rtsopts -fforce-recomp

tests:
	./script.sh init
	./script.sh runtests 0
	./generate-data.sh tests.csv

fast:
	./script.sh init
	./script.sh runtests 1
	./generate-data.sh fast.csv

build-deps:
	cabal install --with-ghc=${GHC} ghc-paths
