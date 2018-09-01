ALL:
	ghc -dynamic test.hs
cls:
	rm -rf test.o test.hi *.bak
clean:
	rm -rf test.o test.hi test *.bak

