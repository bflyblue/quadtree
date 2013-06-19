module Quadtree
where

import Data.Bits

type Vec2 = (Int, Int)

data Direction = NW | NE | SW | SE deriving (Eq, Ord, Bounded, Enum, Show)

data Quad a = Node (Quad a) (Quad a) (Quad a) (Quad a)
            | Empty
            | Leaf a
            | All a
            deriving (Eq, Show)

data Crumb a = Crumb Direction (Quad a) (Quad a) (Quad a)
             deriving (Eq, Show)

data Zipper a = Zipper  { quad          :: Quad a
                        , height        :: Int
                        , breadcrumbs   :: [Crumb a]
                        } deriving (Eq, Show)

up :: (Ord a) => Zipper a -> Maybe (Zipper a)
up (Zipper _ _ []    ) = Nothing
up (Zipper q h (b:bs)) =
    let q' = case b of  Crumb NW ne sw se -> collapse q  ne sw se
                        Crumb NE nw sw se -> collapse nw q  sw se
                        Crumb SW nw ne se -> collapse nw ne q  se
                        Crumb SE nw ne sw -> collapse nw ne sw q
    in Just $ Zipper q' h' bs
    where   collapse Empty Empty Empty Empty = Empty
            collapse (Leaf s) (Leaf t) (Leaf u) (Leaf v)
                | s == t && t == u && u == v = All s
            collapse nw    ne    sw    se    = Node nw ne sw se
            h' = h + 1

dn :: Direction -> Zipper a -> Maybe (Zipper a)
dn _ (Zipper _ 0 _ ) = Nothing
dn d (Zipper q h bc) =
    case expand q of
        (Node nw ne sw se) ->
            let (q',b') = case d of NW -> (nw, Crumb NW ne sw se)
                                    NE -> (ne, Crumb NE nw sw se)
                                    SW -> (sw, Crumb SW nw ne se)
                                    SE -> (se, Crumb SE nw ne sw)
            in Just $ Zipper q' h' (b':bc)
        _   -> Nothing
    where   expand Empty    = Node Empty    Empty    Empty    Empty
            expand (All a)  = Node (Leaf a) (Leaf a) (Leaf a) (Leaf a)
            expand nonempty = nonempty
            h'  = h - 1

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

pathDiff :: [Direction] -> [Direction] -> ([Direction], [Direction])
pathDiff []     ds     = ([],ds)
pathDiff ss     []     = (ss,[])
pathDiff (s:ss) (d:ds) = if s == d then pathDiff ss ds else (s:ss,d:ds)

path :: (Ord a) => [Direction] -> Zipper a -> Maybe (Zipper a)
path dest z =
    up' undos z >>= dn' dos
    where   (undos, dos) = pathDiff (pathFromZipper z) dest
            up' []    z' = Just z'
            up' (_:r) z' = up z' >>= up' r
            dn' []    z' = Just z'
            dn' (s:r) z' = dn s z' >>= dn' r

top :: (Ord a) => Zipper a -> Maybe (Zipper a)
top = path []

to :: (Ord a) => Vec2 -> Zipper a -> Maybe (Zipper a)
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
