all: daf

daf: Main.hs CounterBalance.hs jack.o
	ghc -threaded -O2 --make Main.hs -debug jack.o -ljack -lsndfile -lpthread

jack.o: jack.c
	gcc -c jack.c -ljack

clean: 
	rm *.o *.hi Main
