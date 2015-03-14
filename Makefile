run:
	ghc -c -threaded -O Core.hs
	ghc -c -threaded -O ForDynamiclyLoad.hs
	cabal run

clean:
	cat .gitignore | xargs rm -rf
