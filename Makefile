ALL:
	ghc -dynamic test.hs
clean:
	rm -rf test.o test.hi test *.bak

