default: check

hmmTest: *.hs
	ghc -o hmmTest --make HmmTest

clean:
	rm -f *.o *.hi hmmTest

check: hmmTest
	./hmmTest
