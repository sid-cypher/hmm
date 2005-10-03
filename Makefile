EXE=

default: check

hmmTest$(EXE) hmmverify$(EXE): *.hs
	ghc -Wall -Werror -o hmmTest --make HmmTest
	ghc -Wall -Werror -o hmmverify --make HmmVerify

clean:
	rm -f *.o *.hi hmmTest$(EXE) hmmverify$(EXE)

check: hmmTest$(EXE)
	./hmmTest$(EXE)
