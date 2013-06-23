module Quadtree
where

import Data.Bits
import Data.Word

data Quadtree a = Quadtree !Int !(Quad a)
                deriving (Eq, Show)

data Quad a = Node !(Quad a) !(Quad a) !(Quad a) !(Quad a)
            | Empty
            | Leaf !a
            deriving (Eq, Show)

data Direction = NW | NE | SW | SE deriving (Eq, Ord, Bounded, Enum, Show)

type Scalar = Word8
type Vec2   = (Scalar, Scalar)

expand :: Quad a -> Quad a
expand Empty    = Node Empty Empty Empty Empty
expand (Leaf v) = Node (Leaf v) (Leaf v) (Leaf v) (Leaf v)
expand n        = n

collapse :: Eq a => Quad a -> Quad a
collapse (Node a b c d) | all (==a) [b,c,d] = a
collapse n                                  = n

findQuad :: Eq a => [Direction] -> Quad a -> Quad a
findQuad (NW:ds) (Node nw _  _  _ ) = findQuad ds nw
findQuad (NE:ds) (Node _  ne _  _ ) = findQuad ds ne
findQuad (SW:ds) (Node _  _  sw _ ) = findQuad ds sw
findQuad (SE:ds) (Node _  _  _  se) = findQuad ds se
findQuad _       n                  = n

modifyQuad :: Eq a => (Quad a -> Quad a) -> [Direction] -> Quad a -> Quad a
modifyQuad f []     = f
modifyQuad f (d:ds) = collapse . modify' . expand
    where   modify' (Node nw ne sw se) =
                case d of   NW -> Node (modifyQuad f ds nw) ne sw se
                            NE -> Node nw (modifyQuad f ds ne) sw se
                            SW -> Node nw ne (modifyQuad f ds sw) se
                            SE -> Node nw ne sw (modifyQuad f ds se)
            modify' _ = error "expand didn't return node"


step :: (Bool, Bool) -> Direction
step (False, False) = NW
step (True , False) = NE
step (False, True ) = SW
step (True , True ) = SE

posbits :: Int -> Vec2 -> [(Bool, Bool)]
posbits 0 _     = []
posbits h (x,y) = (testBit x h', testBit y h'):posbits h' (x,y)
                  where h' = h - 1

at :: Int -> Vec2 -> [Direction]
at h pos = map step (posbits h pos)

modify :: Eq a => (Quad a -> Quad a) -> Vec2 -> Quadtree a -> Quadtree a
modify f pos (Quadtree h q) = Quadtree h . modifyQuad f (at h pos) $ q

set :: Eq a => Quad a -> Vec2 -> Quadtree a -> Quadtree a
set v = modify (const v)

insert :: Eq a => a -> Vec2 -> Quadtree a -> Quadtree a
insert = set . Leaf

delete :: Eq a => Vec2 -> Quadtree a -> Quadtree a
delete = set Empty

lookup :: Eq a => Vec2 -> Quadtree a -> Quad a
lookup pos (Quadtree h q) = findQuad (at h pos) q

find :: Eq a => Vec2 -> Quadtree a -> a
find pos q =
    case Quadtree.lookup pos q of
        Leaf v  -> v
        _       -> error "find: not leaf"

findDefault :: Eq a => a -> Vec2 -> Quadtree a -> a
findDefault dflt pos q =
    case Quadtree.lookup pos q of
        Leaf v  -> v
        _       -> dflt

orderPos :: (Vec2, Vec2) -> (Vec2, Vec2)
orderPos ((a,b),(c,d)) | a <= c && b <= d = ((a,b),(c,d))
                       | a <= c && b >  d = ((a,d),(c,b))
                       | a >  c && b <= d = ((c,b),(a,d))
                       | a >  c && b >  d = ((c,d),(a,b))

clampN = map (\x -> case x of   SW -> NW
                                SE -> NE
                                _  -> x )
clampS = map (\x -> case x of   NW -> SW
                                NE -> SE
                                _  -> x )
clampW = map (\x -> case x of   NE -> NW
                                SE -> SW
                                _  -> x )
clampE = map (\x -> case x of   NW -> NE
                                SW -> SE
                                _  -> x )

atR :: Int -> (Vec2, Vec2) -> [[Direction]]
atR h r = atR' (at' NW (a,b), at' NE (c,b), at' SW (a,d), at' SE (c,d))
    where   ((a,b),(c,d)) = orderPos r
            at' rd pos = reverse $ dropWhile (==rd) $ reverse $ at h pos
            atR' ( [],  [],  [],  []) = [[]]
            atR' (is', ks', js', ls') =
                let (i,is) = match NW is'
                    (j,js) = match NE js'
                    (k,ks) = match SW ks'
                    (l,ls) = match SE ls'
                in  case (i, j, k, l) of
                        (NW, NE, SW, SE) -> map (NW:) (atR' (is, clampE js, clampS ks, [])) ++
                                            map (NE:) (atR' (clampW is, js, [], clampS ls)) ++
                                            map (SW:) (atR' (clampN is, [], ks, clampE ls)) ++
                                            map (SE:) (atR' ([], clampN js, clampW ks, ls))

                        (NW, NE, NW, NE) -> map (NW:) (atR' (is, clampE js, ks, clampE ls)) ++
                                            map (NE:) (atR' (clampW is, js, clampW ks, ls))

                        (SW, SE, SW, SE) -> map (SW:) (atR' (is, clampE js, ks, clampE ls)) ++
                                            map (SE:) (atR' (clampW is, js, clampW ks, ls))

                        (NW, NW, SW, SW) -> map (NW:) (atR' (is, js, clampS ks, clampS ls)) ++
                                            map (SW:) (atR' (is, js, clampN ks, clampN ls))

                        (NE, NE, SE, SE) -> map (NE:) (atR' (is, js, clampS ks, clampS ls)) ++
                                            map (SE:) (atR' (is, js, clampN ks, clampN ls))

                        (NW, NW, NW, NW) -> map (NW:) (atR' (is, js, ks, ls))
                        (NE, NE, NE, NE) -> map (NE:) (atR' (is, js, ks, ls))
                        (SW, SW, SW, SW) -> map (SW:) (atR' (is, js, ks, ls))
                        (SE, SE, SE, SE) -> map (SE:) (atR' (is, js, ks, ls))
                        _                -> error $ "atR broken" ++ show [is', js', ks', ls']
                        -- _                -> [[]]

            match md []     = (md,[])
            match _  (x:xs) = (x,xs)


modifyRange :: Eq a => (Quad a -> Quad a) -> (Vec2,Vec2) -> Quadtree a -> Quadtree a
modifyRange f rng (Quadtree h q) = Quadtree h .  foldl (.) id (map (modifyQuad f) (atR h rng)) $ q

setRange :: Eq a => Quad a -> (Vec2,Vec2) -> Quadtree a -> Quadtree a
setRange v = modifyRange (const v)

insertRange :: Eq a => a -> (Vec2,Vec2) -> Quadtree a -> Quadtree a
insertRange = setRange . Leaf

deleteRange :: Eq a => (Vec2,Vec2) -> Quadtree a -> Quadtree a
deleteRange = setRange Empty
