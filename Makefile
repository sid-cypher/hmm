EXE=

default: check

hmmTest$(EXE) hmmverify$(EXE): *.hs
	ghc -Wall -Werror -O -o hmmTest --make HmmTest
	ghc -Wall -Werror -O -o hmmverify --make HmmVerify

clean:
	rm -f *.o *.hi hmmTest$(EXE) hmmverify$(EXE)

check: hmmTest$(EXE)
	./hmmTest$(EXE)

sitecopy: clean
	if [[ "${PWD}" != "${HOME}/projects/hmm" ]]; then false; fi
	echo "Options +Indexes" > .htaccess
	sitecopy -u hmm-repo
