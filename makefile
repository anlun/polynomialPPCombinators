all:
	ghc Doc.hs Format.hs DocHashMap.hs ClearUU.hs MapBURS.hs test.hs -o test

clean:
	rm *.o *.hi test
