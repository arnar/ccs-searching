##
## Makefile
##  
## Made by Arnar Birgisson


all: ccsearch

ccsearch: CCS.hs Console.hs Main.hs Parser.hs Search.hs Semantics.hs
	ghc --make -O -o ccsearch Main.hs

clean:
	-$(RM) ccsearch *.o *~


