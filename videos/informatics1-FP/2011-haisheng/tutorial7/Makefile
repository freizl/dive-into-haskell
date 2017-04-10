.PTHON: build

default: build

pconf=-package-conf ../../cabal-dev/packages-7.4.1.conf/
build:
	ghc ${pconf} tutorial7.hs -o main

ghci:
	ghci ${pconf} tutorial7.hs
