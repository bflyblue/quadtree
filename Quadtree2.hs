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
go UP z                   = up z
go _  (Zipper _     0 _ ) = Nothing
go NW (Zipper Empty h []) = Just $ Zipper Empty (h - 1) []
go d  (Zipper q     h bc) =
    case expand q of
        (Node nw ne sw se) -> let (q',b') = case d of NW -> (nw, Crumb NW ne sw se)
                                                      NE -> (ne, Crumb NE nw sw se)
                                                      SW -> (sw, Crumb SW nw ne se)
                                                      SE -> (se, Crumb SE nw ne sw)
                              in  Just $ Zipper q' (h - 1) (b':bc)
        _                  -> Nothing

-- walk :: Ord a => Zipper a -> [Direction] -> Maybe (Zipper a)
-- walk = foldM (flip go)

step :: (Int, Int) -> Direction
step (0,0) = NW
step (1,0) = NE
step (0,1) = SW
step (1,1) = SE

bits :: Vec2 -> [(Int, Int)]
bits (0,0) = []
bits (x,y) = (lb x, lb y):bits (sr x, sr y)
    where   sr i = shiftR i 1
            lb j = j .&. 1

type Path = (Int, [Direction])

pathForPos :: Vec2 -> Path
--pathForPos = foldl (\(h,d) s -> (h+1,s:d)) (0,[]) . map step . bits
pathForPos = (\x -> (length x, x)) . map step . reverse . bits

pathFromZipper :: Zipper a -> Path
pathFromZipper (Zipper _ h bc) = (h, map dir bc)
    where   dir (Crumb d _ _ _) = d

pathTo :: [Direction] -> [Direction] -> [Direction]
pathTo []     ds     = ds
pathTo ss     []     = map (const UP) ss
pathTo (s:ss) (d:ds) | s == d    = pathTo ss ds
                     | otherwise = pathTo (s:ss) [] ++ pathTo [] (d:ds)

-- path :: Ord a => [Direction] -> Zipper a -> Maybe (Zipper a)
-- path dest z = walk z $ pathTo (pathFromZipper z) dest

-- top :: Ord a => Zipper a -> Maybe (Zipper a)
-- top = path []

-- to :: Ord a => Vec2 -> Zipper a -> Maybe (Zipper a)
-- to p z@(Zipper _ h bc) = path dest z
--     where dest = pathForPos (h + length bc) p

-- view :: Zipper a -> Maybe (Quad a)
-- view (Zipper q h _) =
--     case h of
--         0   -> Just q
--         _   -> Nothing

-- set :: Quad a -> Zipper a -> Maybe (Zipper a)
-- set v (Zipper _ h bc) =
--     case h of
--         0   -> Just (Zipper v h bc)
--         _   -> Nothing

-- modify :: (Quad a -> Quad a) -> Zipper a -> Maybe (Zipper a)
-- modify f (Zipper q h bc) =
--     case h of
--         0   -> Just (Zipper (f q) h bc)
--         _   -> Nothing

-- zipper :: Zipper a
-- zipper = Zipper Empty 0 []
