all: test t

test: test.hs
	ghc -O3 test.hs

t: t.hs
	ghc -O3 t.hs

