all: daf

daf: Main.hs
	cabal install
	cp .cabal-sandbox/bin/daf .
clean: 
	rm *.o *.hi Main *.log
