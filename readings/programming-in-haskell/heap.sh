ghc -prof -fprof-auto -rtsopts heap.hs
##ghc --make -O2 -prof -auto-all -rtsopts heap.hs

./heap +RTS -sstderr -K500M
