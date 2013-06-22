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

modifyQuadR :: Eq a => (Quad a -> Quad a) -> ([Direction], [Direction]) -> Quad a -> Quad a
modifyQuadR f (as,bs) Empty    | fullcoverage as bs = f Empty
modifyQuadR f (as,bs) (Leaf a) | fullcoverage as bs = f (Leaf a)
modifyQuadR f (a:as,b:bs) q = collapse . modify' . expand $ q
    where   modify' (Node nw ne sw se) =
                case (a,b) of
                    (NW,NW) -> Node (m' nw) ne sw se
                    (NE,NE) -> Node nw (m' ne) sw se
                    (SW,SW) -> Node nw se (m' sw) se
                    (SE,SE) -> Node nw se sw (m' se)

                    (NW,NE) -> m' nw `par` m' ne `pseq` Node (m' nw) (m' ne) sw se
                    (SW,SE) -> m' sw `par` m' se `pseq` Node nw ne (m' sw) (m' se)
                    (NW,SW) -> m' nw `par` m' sw `pseq` Node (m' nw) ne (m' sw) se
                    (NE,SE) -> m' ne `par` m' se `pseq` Node nw (m' ne) sw (m' se)

                    (NW,SE) -> m' nw `par` m' ne `par` m' sw `par` m' se `pseq`
                               Node (m' nw) (m' ne) (m' sw) (m' se)
            m' = modifyQuadR f (as,bs)
modifyQuadR f _           q = f q

setQuadR :: Eq a => Quad a -> ([Direction], [Direction]) -> Quad a -> Quad a
setQuadR v (as,bs) Empty    | fullcoverage as bs = v
setQuadR v (as,bs) (Leaf _) | fullcoverage as bs = v
setQuadR v (_,[])      _ = v
setQuadR v (a:as,b:bs) q = collapse . set' . expand $ q
    where   set' (Node nw ne sw se) =
                case (a,b) of
                    (NW,NW) -> Node (s' nw) ne sw se
                    (NE,NE) -> Node nw (s' ne) sw se
                    (SW,SW) -> Node nw se (s' sw) se
                    (SE,SE) -> Node nw se sw (s' se)

                    (NW,NE) -> s' nw `par` s' ne `pseq` Node (s' nw) (s' ne) sw se
                    (SW,SE) -> s' sw `par` s' se `pseq` Node nw ne (s' sw) (s' se)
                    (NW,SW) -> s' nw `par` s' sw `pseq` Node (s' nw) ne (s' sw) se
                    (NE,SE) -> s' ne `par` s' se `pseq` Node nw (s' ne) sw (s' se)

                    (NW,SE) -> if fullcoverage as bs
                               then v
                               else s' nw `par` s' ne `par` s' sw `par` s' se `pseq`
                                    Node (s' nw) (s' ne) (s' sw) (s' se)
            s' = setQuadR v (as,bs)
setQuadR v _           _ = v

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

