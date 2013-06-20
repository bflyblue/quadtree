module Quadtree
where

import Control.Monad
import Data.Bits

type Vec2 = (Int, Int)

data Direction = UP | NW | NE | SW | SE deriving (Eq, Ord, Bounded, Enum, Show)

data Quad a = Node (Quad a) (Quad a) (Quad a) (Quad a)
            | Empty
            | Leaf a
            deriving (Eq, Show)

data Crumb a = Crumb Direction (Quad a) (Quad a) (Quad a)
             deriving (Eq, Show)

data Zipper a = Zipper  { quad          :: Quad a
                        , height        :: Int
                        , breadcrumbs   :: [Crumb a]
                        } deriving (Eq, Show)

collapse :: Eq a => Quad a -> Quad a
collapse (Node a b c d) | all (==a) [b,c,d] = a
collapse other                              = other

expand :: Quad a -> Quad a
expand Empty    = Node Empty    Empty    Empty    Empty
expand (Leaf a) = Node (Leaf a) (Leaf a) (Leaf a) (Leaf a)
expand other    = other

up :: Ord a => Zipper a -> Maybe (Zipper a)
up (Zipper q h []    ) = Just $ Zipper (Node q Empty Empty Empty) (h + 1) []
up (Zipper q h (b:bs)) =
    let q' = (collapse . decrumb) b
    in  Just $ Zipper q' (h + 1) bs
    where   decrumb (Crumb NW ne sw se) = Node q  ne sw se
            decrumb (Crumb NE nw sw se) = Node nw q  sw se
            decrumb (Crumb SW nw ne se) = Node nw ne q  se
            decrumb (Crumb SE nw ne sw) = Node nw ne sw q

go :: Ord a => Direction -> Zipper a -> Maybe (Zipper a)
go UP z               = up z
go _  (Zipper _ 0 _ ) = Nothing
go d  (Zipper q h bc) =
    case expand q of
        (Node nw ne sw se) -> let (q',b') = case d of NW -> (nw, Crumb NW ne sw se)
                                                      NE -> (ne, Crumb NE nw sw se)
                                                      SW -> (sw, Crumb SW nw ne se)
                                                      SE -> (se, Crumb SE nw ne sw)
                              in  Just $ Zipper q' (h - 1) (b':bc)
        _                  -> Nothing

walk :: Ord a => Zipper a -> [Direction] -> Maybe (Zipper a)
walk = foldM (flip go)

step :: (Bool, Bool) -> Direction
step (False, False) = NW
step (True , False) = NE
step (False, True ) = SW
step (True , True ) = SE

pathForPos :: Int -> Vec2 -> [Direction]
pathForPos 0 _     = []
pathForPos h (x,y) = step (xb,yb):pathForPos h' (x,y)
    where   h' = h - 1
            xb = testBit x h'
            yb = testBit y h'

pathFromZipper :: Zipper a -> [Direction]
pathFromZipper (Zipper _ _ bc) =
    map dir bc
    where   dir (Crumb d _ _ _) = d

pathTo :: [Direction] -> [Direction] -> [Direction]
pathTo []     ds     = ds
pathTo ss     []     = map (const UP) ss
pathTo (s:ss) (d:ds) | s == d   = pathTo ss ds
                     |otherwise = pathTo (s:ss) [] ++ pathTo [] (d:ds)

path :: Ord a => [Direction] -> Zipper a -> Maybe (Zipper a)
path dest z = walk z $ pathTo (pathFromZipper z) dest

top :: Ord a => Zipper a -> Maybe (Zipper a)
top = path []

to :: Ord a => Vec2 -> Zipper a -> Maybe (Zipper a)
to p z@(Zipper _ h bc) = path dest z
    where dest = pathForPos (h + length bc) p

view :: Zipper a -> Maybe (Quad a)
view (Zipper q h _) =
    case h of
        0   -> Just q
        _   -> Nothing

set :: Quad a -> Zipper a -> Maybe (Zipper a)
set v (Zipper _ h bc) =
    case h of
        0   -> Just (Zipper v h bc)
        _   -> Nothing

modify :: (Quad a -> Quad a) -> Zipper a -> Maybe (Zipper a)
modify f (Zipper q h bc) =
    case h of
        0   -> Just (Zipper (f q) h bc)
        _   -> Nothing

zipper :: Zipper a
zipper = Zipper Empty 0 []
