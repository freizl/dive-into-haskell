clean:
	rm -f *.hi *.o *.dyn_hi *.dyn_o

build: clean
	ghc --make Main.hs Hello.hs

run: build
	./Main
