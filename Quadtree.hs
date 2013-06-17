module Quadtree
where

import Control.Monad
import Data.Bits
import Data.Ratio

type Vec2 = (Int, Int)

data Quad a = Node Int      (Quad a) (Quad a)
                            (Quad a) (Quad a)
            | Empty Int
            | Leaf a
            deriving (Eq, Show)

data Crumb a = NWCrumb Int           (Quad a)
                            (Quad a) (Quad a)
             | NECrumb Int  (Quad a)
                            (Quad a) (Quad a)
             | SWCrumb Int  (Quad a) (Quad a)
                                     (Quad a)
             | SECrumb Int  (Quad a) (Quad a)
                            (Quad a)
             deriving (Eq, Show)

data Direction = NW | NE | SW | SE | N | S | W | E | UP
               deriving (Eq, Enum, Show, Bounded)

type Breadcrumbs a = [Crumb a]

data Zipper a = Zipper (Quad a) (Breadcrumbs a)
              deriving (Eq, Show)

qlevel :: Quad a -> Int
qlevel (Node  k _ _ _ _) = k
qlevel (Empty k)         = k
qlevel (Leaf  _)         = 0

top :: Quad a -> Zipper a
top q = Zipper q []

quad :: Zipper a -> Quad a
quad (Zipper q _) = q

modify :: (Quad a -> Quad a) -> Zipper a -> Zipper a
modify f (Zipper q bs) = Zipper (f q) bs

go :: Direction -> Zipper a -> Maybe (Zipper a)
go UP z = goUp z
go NW z = goDn NW z
go NE z = goDn NE z
go SW z = goDn SW z
go SE z = goDn SE z
go N z  = step N z
go S z  = step S z
go W z  = step W z
go E z  = step E z

goUp :: Zipper a -> Maybe (Zipper a)
goUp (Zipper _  [])                      = Nothing
goUp (Zipper nw (NWCrumb k ne sw se : bs)) = Just $ Zipper (Node k nw ne sw se) bs
goUp (Zipper ne (NECrumb k nw sw se : bs)) = Just $ Zipper (Node k nw ne sw se) bs
goUp (Zipper sw (SWCrumb k nw ne se : bs)) = Just $ Zipper (Node k nw ne sw se) bs
goUp (Zipper se (SECrumb k nw ne sw : bs)) = Just $ Zipper (Node k nw ne sw se) bs

goDn :: Direction -> Zipper a -> Maybe (Zipper a)
goDn d zipper =
    case zipper of
        Zipper (Leaf _) _           -> Nothing
        Zipper (Node 0 _ _ _ _) _   -> Nothing
        Zipper (Empty 0) _          -> Nothing
        Zipper (Empty k) bs         -> goDn d (Zipper emptynode bs)
            where   emptynode = Node k e e e e
                    k'        = k - 1
                    e         = Empty k'
        Zipper (Node k nw ne sw se) bs ->
            case d of
                NW -> Just $ Zipper nw (NWCrumb k ne sw se : bs)
                NE -> Just $ Zipper ne (NECrumb k nw sw se : bs)
                SW -> Just $ Zipper sw (SWCrumb k nw ne se : bs)
                SE -> Just $ Zipper se (SECrumb k nw ne sw : bs)

walk :: Zipper a -> [Direction] -> Maybe (Zipper a)
walk = foldM (flip go)

step :: Direction -> Zipper a -> Maybe (Zipper a)
step d zipper =
    case zipper of
        Zipper _ []     -> Nothing
        Zipper _ (b:_)  ->
            case d of
                N -> case b of
                    NWCrumb{} -> walk zipper [UP, N, SW]
                    NECrumb{} -> walk zipper [UP, N, SE]
                    SWCrumb{} -> walk zipper [UP, NW]
                    SECrumb{} -> walk zipper [UP, NE]
                S -> case b of
                    NWCrumb{} -> walk zipper [UP, SW]
                    NECrumb{} -> walk zipper [UP, SE]
                    SWCrumb{} -> walk zipper [UP, S, NW]
                    SECrumb{} -> walk zipper [UP, S, NE]
                W -> case b of
                    NWCrumb{} -> walk zipper [UP, W, NE]
                    NECrumb{} -> walk zipper [UP, NW]
                    SWCrumb{} -> walk zipper [UP, W, SE]
                    SECrumb{} -> walk zipper [UP, SW]
                E -> case b of
                    NWCrumb{} -> walk zipper [UP, NE]
                    NECrumb{} -> walk zipper [UP, E, NW]
                    SWCrumb{} -> walk zipper [UP, SE]
                    SECrumb{} -> walk zipper [UP, E, SW]

topmost :: Zipper a -> Zipper a
topmost zipper =
    case goUp zipper of
        Just z' -> topmost z'
        Nothing -> zipper

lowest :: [Direction] -> Zipper a -> Zipper a
lowest (d:ds) zipper =
    case go d zipper of
        Just z' -> lowest ds z'
        Nothing -> zipper

pathTo :: Vec2 -> Int -> [Direction]
pathTo _        0 = []
pathTo pt@(x,y) k =
    case p of
        (False, False) -> NW:pathTo pt nk
        (True , False) -> NE:pathTo pt nk
        (False, True ) -> SW:pathTo pt nk
        (True , True ) -> SE:pathTo pt nk
    where   nk = k - 1
            x' = testBit x nk
            y' = testBit y nk
            p  = (x',y')

applyByPath :: (Quad a -> Quad a) -> [Direction] -> Zipper a -> Zipper a
applyByPath f path zipper =
    let dest = foldM (flip go) zipper path
    in  case dest of
            Just q  -> modify f q
            Nothing -> zipper

empty :: Int -> Quad a
empty = Empty

insert :: Vec2 -> a -> Quad a -> Quad a
insert pt val q =
    quad $ topmost (applyByPath insert' (pathTo pt k) (top q))
    where   k = qlevel q
            insert' _ = Leaf val

delete :: Vec2 -> Quad a -> Quad a
delete pt q =
    quad $ topmost (applyByPath delete' (pathTo pt k) (top q))
    where   k = qlevel q
            delete' _ = Empty k

insertList :: Quad a -> [(Vec2, a)] -> Quad a
insertList = foldl (\acc (pt,val) -> insert pt val acc)

raytrace :: Quad a -> Vec2 -> Vec2 -> [(Vec2, a)]
raytrace q pt (dx,dy)
    | dx == 0 && dy == 0 = []
    | otherwise          =
        case q of
            Empty _         -> []
            Leaf  a         -> [(pt, a)]
            Node  k _ _ _ _ ->
                let initz = lowest (pathTo pt k) (top q) in
                    if abs dx > abs dy
                    then stepx initz pt (dy % dx)
                    else stepy initz pt (dx % dy)
    where   stepx z (x,y) y' = []
            stepy z (x,y) x' = []

