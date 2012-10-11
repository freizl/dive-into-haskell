
PROG  = ./hakyll
SOURCES = hakyll.hs
HC=ghc

SITE=_site
CACHE=_cache

default: preview

$(PROG) : $(SOURCES)
	$(HC) --make $< -o $@

clean:
	rm -f $(SOURCES:.hs=.hi) $(SOURCES:.hs=.o) $(PROG)
	rm -rf $(SITE) $(CACHE)
	rm -rf $(GAE)/$(SITE)

build: $(PROG)
	$(PROG) build

rebuild: $(PROG)
	$(PROG) rebuild

preview:
	$(PROG) preview 9900

deploy: build
	$(PROG) deploy

p: preview

bp: build preview

rp: rebuild preview

