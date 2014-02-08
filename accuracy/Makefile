GHC=/home/hs01/ezyang/ghc-alloc/inplace/bin/ghc-stage2

all:
	${GHC} --make Main.hs -package ghc -o damp

build-deps:
	cabal install --with-ghc=${GHC} ghc-paths
