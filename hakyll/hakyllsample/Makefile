
PROG  = hakyll
SOURCES = hakyll.hs

SITE=_site
CACHE=_cache

GAE=gae

default: rebuild

$(PROG) : $(SOURCES)
	$(HC) --make $< -o $@ $(HCFLAGS)

clean:
	rm -f $(SOURCES:.hs=.hi) $(SOURCES:.hs=.o) $(PROG)
	rm -rf $(SITE) $(CACHE)
	rm -rf $(GAE)/$(SITE)

build: $(PROG)
	./$(PROG) build

rebuild: $(PROG)
	./$(PROG) rebuild

deploy: rebuild
	cp -R $(SITE) $(GAE)

HC=ghc
