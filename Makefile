all: daf

daf: Main.hs CounterBalance.hs jack.o gltext.o
	ghc -threaded -O2 --make Main.hs -debug jack.o gltext.o -ljack -lsndfile -lpthread -lGL -LGLEW

gltext.o: gltext.c
	gcc -c gltext.c -I freetype-gl

jack.o: jack.c
	gcc -c jack.c

clean: 
	rm *.o *.hi Main *.log
