run:
	ghc -c -threaded -dynamic -O ForDynamiclyLoad.hs
	cabal run
