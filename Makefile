EXE=

default: hmmverify$(EXE) hmmprint$(EXE) check doc

hmmTest$(EXE) hmmverify$(EXE) hmmprint$(EXE): *.hs
	ghc -Wall -Werror -O -o hmmTest --make HmmTest
	ghc -Wall -Werror -O -o hmmverify --make HmmVerify
	ghc -Wall -Werror -O -o hmmprint --make HmmPrint

hghtest$(EXE): *.lhs
	ghc -Wall -Werror -O -o hghtest --make HghTest

doc: HghCore.xhtml

distclean::
	rm -f *.o *.hi
	rm -f hmmTest$(EXE) hmmverify$(EXE) hmmprint$(EXE)
	rm -f hghtest$(EXE)

clean:: distclean
	rm -f .htaccess
	rm -f *.xhtml

check: hmmTest$(EXE) hghtest$(EXE) doc
	./hmmTest$(EXE)
	./hghtest$(EXE)

sitecopy: clean sitecopy-is-safe doc
	echo "Options +Indexes" > .htaccess
	sitecopy -u hmm-repo

sitecopy-is-safe:
	if [[ "${PWD}" != "${HOME}/projects/hmm" ]]; then false; fi
	@test "$$(darcs whatsnew --look-for-adds 2>&1)" = 'No changes!' \
		|| (echo "You have unrecorded changes!" && false)

%.xhtml: %.lhs
	rst2html.py --exit-status=info --report=info -s -g $< $@ || (rm -f $@ && false)

