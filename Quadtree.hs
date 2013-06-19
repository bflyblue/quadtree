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

step :: (Bool, Bool) -> Direction
step (False, False) = NW
step (True , False) = NE
step (False, True ) = SW
step (True , True ) = SE

path :: Int -> Vec2 -> [Direction]
path 0 _     = []
path h (x,y) = step (xb,yb):path h' (x,y)
    where   h' = h - 1
            xb = testBit x h'
            yb = testBit y h'

expand :: Quad a -> Quad a
expand Empty    = Node Empty Empty Empty Empty
expand (Leaf a) = Node (Leaf a) (Leaf a) (Leaf a) (Leaf a)
expand n        = n

collapse :: Eq a => Quad a -> Quad a
collapse (Node (Leaf a) (Leaf b) (Leaf c) (Leaf d))
                       | a == b && b == c && c == d = Leaf a
collapse (Node Empty    Empty    Empty    Empty   ) = Empty
collapse n                                          = n

modifyPath :: Eq a => (Quad a -> Quad a) -> [Direction] -> Quad a -> Quad a
modifyPath f []     = f
modifyPath f (d:ds) =
    collapse . modify' . expand
    where modify' (Node nw ne sw se) =
            case d of   NW -> Node (modifyPath f ds nw) ne sw se
                        NE -> Node nw (modifyPath f ds ne) sw se
                        SW -> Node nw ne (modifyPath f ds sw) se
                        SE -> Node nw ne sw (modifyPath f ds se)

modify :: Eq a => (Quad a -> Quad a) -> Vec2 -> Quadtree a -> Quadtree a
modify f pos (Quadtree h q) = Quadtree h $ modifyPath f (path h pos) q

insertPath :: Eq a => a -> [Direction] -> Quad a -> Quad a
insertPath v = modifyPath (\_ -> Leaf v)

insert :: Eq a => a -> Vec2 -> Quadtree a -> Quadtree a
insert v = modify (\_ -> Leaf v)

deletePath :: Eq a => [Direction] -> Quad a -> Quad a
deletePath = modifyPath (const Empty)

delete :: Eq a => Vec2 -> Quadtree a -> Quadtree a
delete = modify (const Empty)

