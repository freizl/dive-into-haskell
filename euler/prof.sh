ghc --make -prof -auto-all -rtsopts -o p $1 && time ./p +RTS -p -RTS
