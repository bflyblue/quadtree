all: test t

test: test.hs Quadtree.hs
	ghc -O3 test.hs

t: t.hs Quadtree.hs
	ghc -O3 t.hs

