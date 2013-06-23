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

atR :: Int -> (Vec2, Vec2) -> [[Direction]]
atR h r = atR' (at' NW (a,b), at' NE (c,b), at' SW (a,d), at' SE (c,d))
    where   ((a,b),(c,d)) = orderPos r
            at' rd pos = reverse $ dropWhile (==rd) $ reverse $ at h pos
            atR' (   [],     _,
                      _,    [])             = [[]]
            atR' (    _,    [],
                     [],     _)             = [[]]
            atR' (i:is,   js,   [],   ks) = map ( i:) (atR' (is, js, [], ks))
            atR' (  is, j:js,   [],   ks) = map ( j:) (atR' (is, js, [], ks))
            atR' (  is,   [], k:ks,   ls) = map ( k:) (atR' (is, [], ks, ls))
            atR' (  [],   js,   ks, l:ls) = map ( l:) (atR' ([], js, ks, ls))
            atR' (i:is, j:js,   [],   [])
                              | i == j    = map ( i:) (atR' (is, js, [], []))
                              | otherwise = map ( i:) (atR' (is, [], [], [])) ++
                                            map ( j:) (atR' ([], js, [], []))
            atR' (i:is,   [], k:ks,   [])
                              | i == k    = map ( i:) (atR' (is, [], ks, []))
                              | otherwise = map ( i:) (atR' (is, [], [], [])) ++
                                            map ( k:) (atR' ([], [], ks, []))
            atR' (  [],   [], k:ks, l:ls)
                              | k == l    = map ( k:) (atR' ([], [], ks, ls))
                              | otherwise = map ( k:) (atR' ([], [], ks, [])) ++
                                            map ( l:) (atR' ([], [], [], ls))
            atR' (  [], j:js,   [], l:ls)
                              | j == l    = map ( j:) (atR' ([], js, [], ls))
                              | otherwise = map ( j:) (atR' ([], js, [], [])) ++
                                            map ( l:) (atR' ([], [], [], ls))
            atR' (  [], j:js, k:ks, l:ls) = atR' ([NW], j:js, k:ks, l:ls)
            atR' (i:is,   [], k:ks, l:ls) = atR' (i:is, [NE], k:ks, l:ls)
            atR' (i:is, j:js,   [], l:ls) = atR' (i:is, j:js, [SW], l:ls)
            atR' (i:is, j:js, k:ks,   []) = atR' (i:is, j:js, k:ks, [SE])
            atR' (i:is, j:js, k:ks, l:ls)
                              | i == j &&
                                j == k &&
                                k == l    = map ( i:) (atR' (is, js, ks, ls))
                              | i == j &&
                                k == l    = map ( i:) (atR' (is, js, [], [])) ++
                                            map ( k:) (atR' ([], [], ks, ls))
                              | i == k &&
                                j == l    = map ( i:) (atR' (is, [], ks, [])) ++
                                            map ( j:) (atR' ([], js, [], ls))
                              | otherwise = map ( i:) (atR' (is, [], [], [])) ++
                                            map ( j:) (atR' ([], js, [], [])) ++
                                            map ( k:) (atR' ([], [], ks, [])) ++
                                            map ( l:) (atR' ([], [], [], ls))

            -- match md []     = (md,[])
            -- match _  (x:xs) = (x,xs)

modifyRange :: Eq a => (Quad a -> Quad a) -> (Vec2,Vec2) -> Quadtree a -> Quadtree a
modifyRange f rng (Quadtree h q) = Quadtree h .  foldl (.) id (map (modifyQuad f) (atR h rng)) $ q

setRange :: Eq a => Quad a -> (Vec2,Vec2) -> Quadtree a -> Quadtree a
setRange v = modifyRange (const v)

insertRange :: Eq a => a -> (Vec2,Vec2) -> Quadtree a -> Quadtree a
insertRange = setRange . Leaf

deleteRange :: Eq a => (Vec2,Vec2) -> Quadtree a -> Quadtree a
deleteRange = setRange Empty
