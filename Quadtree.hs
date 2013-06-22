module Quadtree
where

import Data.Bits
import Data.Word
import Control.Parallel

data Quadtree a = Quadtree !Int !(Quad a)
                deriving (Eq, Show)

data Quad a = Node !(Quad a) !(Quad a) !(Quad a) !(Quad a)
            | Empty
            | Leaf !a
            deriving (Eq, Show)

data Direction = NW | NE | SW | SE deriving (Eq, Ord, Bounded, Enum, Show)

type Scalar = Word
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
    where modify' (Node nw ne sw se) =
            case d of   NW -> Node (modifyQuad f ds nw) ne sw se
                        NE -> Node nw (modifyQuad f ds ne) sw se
                        SW -> Node nw ne (modifyQuad f ds sw) se
                        SE -> Node nw ne sw (modifyQuad f ds se)

fullcoverage :: [Direction] -> [Direction] -> Bool
fullcoverage as bs = all (== NW) as && all (== SE) bs

partial :: (t -> ([Direction], [Direction]) -> Quad a -> Quad a) -> t -> ([Direction], [Direction]) -> Quad a -> Quad a
partial f g (a:as,b:bs) (Node nw ne sw se) =
    case (a,b) of
        (NW,NW) -> Node (f' nw) ne sw se
        (NE,NE) -> Node nw (f' ne) sw se
        (SW,SW) -> Node nw se (f' sw) se
        (SE,SE) -> Node nw se sw (f' se)

        (NW,NE) -> f' nw `par` f' ne `pseq` Node (f' nw) (f' ne) sw se
        (SW,SE) -> f' sw `par` f' se `pseq` Node nw ne (f' sw) (f' se)
        (NW,SW) -> f' nw `par` f' sw `pseq` Node (f' nw) ne (f' sw) se
        (NE,SE) -> f' ne `par` f' se `pseq` Node nw (f' ne) sw (f' se)

        _       -> f' nw `par` f' ne `par` f' sw `par` f' se `pseq` Node (f' nw) (f' ne) (f' sw) (f' se)
    where f' = f g (as,bs)

modifyQuadR :: Eq a => (Quad a -> Quad a) -> ([Direction], [Direction]) -> Quad a -> Quad a
modifyQuadR f (as,bs) Empty    | fullcoverage as bs = f Empty
modifyQuadR f (as,bs) (Leaf a) | fullcoverage as bs = f (Leaf a)
modifyQuadR f (as,bs) q        = collapse . partial modifyQuadR f (as,bs) . expand $ q

setQuadR :: Eq a => Quad a -> ([Direction], [Direction]) -> Quad a -> Quad a
setQuadR v (as,bs) Empty    | fullcoverage as bs = v
setQuadR v (as,bs) (Leaf _) | fullcoverage as bs = v
setQuadR v (as,bs) q        = collapse . partial setQuadR v (as,bs) . expand $ q

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

orderPos :: (Vec2, Vec2) -> (Vec2, Vec2)
orderPos ((a,b),(c,d)) | a <= c && b <= d = ((a,b),(c,d))
                       | a <= c && b >  d = ((a,d),(c,b))
                       | a >  c && b >  d = ((c,d),(a,b))
                       | a >  c && b >  d = ((c,d),(a,b))

atR :: Int -> (Vec2, Vec2) -> ([Direction], [Direction])
atR h r = let ((a,b),(c,d)) = orderPos r
          in  (at h (a,b), at h (c,d))

modifyRange :: Eq a => (Quad a -> Quad a) -> (Vec2, Vec2) -> Quadtree a -> Quadtree a
modifyRange f r (Quadtree h q) = Quadtree h . modifyQuadR f (atR h r) $ q

setRange :: Eq a => a -> (Vec2, Vec2) -> Quadtree a -> Quadtree a
setRange v r (Quadtree h q) = Quadtree h . setQuadR (Leaf v) (atR h r) $ q

deleteRange :: Eq a => (Vec2, Vec2) -> Quadtree a -> Quadtree a
deleteRange r (Quadtree h q) = Quadtree h . setQuadR Empty (atR h r) $ q

find :: Eq a => Vec2 -> Quadtree a -> a
find pos q =
    case Quadtree.lookup pos q of
        Leaf v  -> v
        _       -> error "find: not leaf"

