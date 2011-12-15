
SOURCES = $1

build : $(SOURCES)
	$(HC) --make -prof -auto-all -rtsopts $@

clean:
	rm -f $(SOURCES:.hs=.hi) $(SOURCES:.hs=.o) $(PROG)

HC=ghc
