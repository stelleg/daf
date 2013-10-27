
all: daf

daf: Main.hs CounterBalance.hs
	ghc -threaded -O2 --make Main.hs

clean: 
	rm *.o *.hi Main
