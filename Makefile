default: check

hmmTest.exe hmmverify.exe: *.hs
	ghc -Wall -Werror -o hmmTest --make HmmTest
	ghc -Wall -Werror -o hmmverify --make HmmVerify

clean:
	rm -f *.o *.hi hmmTest.exe hmmverify.exe

check: hmmTest.exe
	./hmmTest.exe
