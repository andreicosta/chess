all: Main.hs Init.hs Actions.hs Structure.hs
	ghc Main.hs
	rm *.hi
	rm *.o

clean:
	rm Main
