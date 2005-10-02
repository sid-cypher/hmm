default: check

hmmTest: *.hs
	ghc -Wall -Werror -o hmmTest --make HmmTest

clean:
	rm -f *.o *.hi hmmTest

check: hmmTest
	./hmmTest
