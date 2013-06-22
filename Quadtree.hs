module Quadtree
where

import Data.Bits
import Data.Word

type Scalar = Word
type Vec2   = (Scalar, Scalar)

data Direction = NW | NE | SW | SE deriving (Eq, Ord, Bounded, Enum, Show)

data Quadtree a = Quadtree !Int !(Quad a)
                deriving (Eq, Show)

data Quad a = Node !(Quad a) !(Quad a) !(Quad a) !(Quad a)
            | Empty
            | Leaf !a
            deriving (Eq, Show)

expand :: Quad a -> Quad a
expand Empty    = Node Empty Empty Empty Empty
expand (Leaf a) = Node (Leaf a) (Leaf a) (Leaf a) (Leaf a)
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
    where modify' (Node nw ne sw se) =
            case d of   NW -> Node (modifyQuad f ds nw) ne sw se
                        NE -> Node nw (modifyQuad f ds ne) sw se
                        SW -> Node nw ne (modifyQuad f ds sw) se
                        SE -> Node nw ne sw (modifyQuad f ds se)

insertFn :: Quad a -> Quad a -> Quad a
insertFn = const

deleteFn :: Quad a -> Quad a
deleteFn = insertFn Empty

insertQuad :: Eq a => Quad a -> [Direction] -> Quad a -> Quad a
insertQuad v = modifyQuad (insertFn v)

deleteQuad :: Eq a => [Direction] -> Quad a -> Quad a
deleteQuad = modifyQuad deleteFn

step :: (Bool, Bool) -> Direction
step (False, False) = NW
step (True , False) = NE
step (False, True ) = SW
step (True , True ) = SE

posbits :: Int -> Vec2 -> [(Bool, Bool)]
posbits 0 _     = []
posbits h (x,y) = (testBit x h, testBit y h):posbits (h-1) (x,y)

at :: Int -> Vec2 -> [Direction]
at h pos   = map step (posbits h pos)

insert :: Eq a => a -> Vec2 -> Quadtree a -> Quadtree a
insert v pos (Quadtree h q) = Quadtree h $ insertQuad (Leaf v) (at h pos) q

delete :: Eq a => Vec2 -> Quadtree a -> Quadtree a
delete pos (Quadtree h q) = Quadtree h $ deleteQuad (at h pos) q

lookup :: Eq a => Vec2 -> Quadtree a -> a
lookup pos (Quadtree h q) =
    case findQuad (at h pos) q of
        Leaf a  -> a

