module Quadtree
where

import Data.Bits

type Vec2 = (Int, Int)

data Direction = NW | NE | SW | SE deriving (Eq, Ord, Bounded, Enum, Show)

data Quadtree a = Quadtree Int (Quad a)
                deriving (Eq, Show)

data Quad a = Node (Quad a) (Quad a) (Quad a) (Quad a)
            | Empty
            | Leaf a
            deriving (Eq, Show)

expand :: Quad a -> Quad a
expand Empty    = Node Empty Empty Empty Empty
expand (Leaf a) = Node (Leaf a) (Leaf a) (Leaf a) (Leaf a)
expand n        = n

collapse :: Eq a => Quad a -> Quad a
collapse (Node (Leaf a) (Leaf b) (Leaf c) (Leaf d))
                       | a == b && b == c && c == d = Leaf a
collapse (Node Empty    Empty    Empty    Empty   ) = Empty
collapse n                                          = n

findPath :: Eq a => [Direction] -> Quad a -> Quad a
findPath []      q                  = q
findPath _       Empty              = Empty
findPath _       (Leaf a)           = Leaf a
findPath (NW:ds) (Node nw _  _  _ ) = findPath ds nw
findPath (NE:ds) (Node _  ne _  _ ) = findPath ds ne
findPath (SW:ds) (Node _  _  sw _ ) = findPath ds sw
findPath (SE:ds) (Node _  _  _  se) = findPath ds se

modifyPath :: Eq a => (Quad a -> Quad a) -> [Direction] -> Quad a -> Quad a
modifyPath f []     = f
modifyPath f (d:ds) = collapse . modify' . expand
    where modify' (Node nw ne sw se) =
            case d of   NW -> Node (modifyPath f ds nw) ne sw se
                        NE -> Node nw (modifyPath f ds ne) sw se
                        SW -> Node nw ne (modifyPath f ds sw) se
                        SE -> Node nw ne sw (modifyPath f ds se)

over' :: Eq a => (Quad a -> Quad a) -> [Direction] -> Quadtree a -> Quadtree a
over' f pos (Quadtree h q) = Quadtree h $ modifyPath f pos q

set' :: Eq a => a -> [Direction] -> Quadtree a -> Quadtree a
set' v = over' (\_ -> Leaf v)

delete' :: Eq a => [Direction] -> Quadtree a -> Quadtree a
delete' = over' (const Empty)

view' :: Eq a => [Direction] -> Quadtree a -> Quad a
view' pos (Quadtree _ q) = findPath pos q

step :: (Bool, Bool) -> Direction
step (False, False) = NW
step (True , False) = NE
step (False, True ) = SW
step (True , True ) = SE

pathTo' :: Vec2 -> Int -> [Direction]
pathTo' _     0 = []
pathTo' (x,y) h = step (xb,yb):pathTo' (x,y) h'
    where   h' = h - 1
            xb = testBit x h'
            yb = testBit y h'

pathTo :: Vec2 -> Quadtree t -> [Direction]
pathTo pt (Quadtree h _) = pathTo' pt h

over :: Eq a => (Quad a -> Quad a) -> Vec2 -> Quadtree a -> Quadtree a
over f pos qt = over' f (pathTo pos qt) qt

set :: Eq a => a -> Vec2 -> Quadtree a -> Quadtree a
set v pos qt = set' v (pathTo pos qt) qt

delete :: Eq a => Vec2 -> Quadtree a -> Quadtree a
delete pos qt = delete' (pathTo pos qt) qt

view :: Eq a => Vec2 -> Quadtree a -> Quad a
view pos qt = view' (pathTo pos qt) qt
