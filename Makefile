.PHONY: cabal-dev/bin/Shen.hs

cabal-dev/bin/Shen.hs:
	cabal-dev install --enable-executable-profiling --enable-library-profiling --disable-documentation --ghc-option=-fprof-auto

run: cabal-dev/bin/Shen.hs
	rlwrap cabal-dev/bin/Shen.hs +RTS -p -RTS --shen "K Lambda"
