module Quadtree
where

import Control.Monad
import Data.Bits

type Vec2 = (Int, Int)

data Quadtree a = Quadtree Int (Quad a)
                deriving (Eq, Show)

data Quad a = Node (Quad a) (Quad a)
                   (Quad a) (Quad a)
            | Leaf a
            | Empty
            deriving (Eq, Show)

data Crumb a = NWCrumb          (Quad a)
                       (Quad a) (Quad a)
             | NECrumb (Quad a)
                       (Quad a) (Quad a)
             | SWCrumb (Quad a) (Quad a)
                                (Quad a)
             | SECrumb (Quad a) (Quad a)
                       (Quad a)

data Direction = NW | NE | SW | SE | N | S | W | E | UP
               deriving (Eq, Enum, Show, Bounded)

type Breadcrumbs a = [Crumb a]

data Zipper a = Zipper (Quad a) Int (Breadcrumbs a)

top :: Quadtree a -> Zipper a
top (Quadtree k q) = Zipper q k []

quad :: Zipper a -> Quad a
quad (Zipper q _ _) = q

modify :: (Quad a -> Quad a) -> Zipper a -> Zipper a
modify f (Zipper q k bs) = Zipper (f q) k bs

go :: Direction -> Zipper a -> Maybe (Zipper a)
go UP z = goUp z
go NW z = goDn NW z
go NE z = goDn NE z
go SW z = goDn SW z
go SE z = goDn SE z
go N z = step N z
go S z = step S z
go W z = step W z
go E z = step E z

goUp :: Zipper a -> Maybe (Zipper a)
goUp (Zipper _  _ [])                      = Nothing
goUp (Zipper nw k (NWCrumb ne sw se : bs)) = Just $ Zipper (Node nw ne sw se) (k+1) bs
goUp (Zipper ne k (NECrumb nw sw se : bs)) = Just $ Zipper (Node nw ne sw se) (k+1) bs
goUp (Zipper sw k (SWCrumb nw ne se : bs)) = Just $ Zipper (Node nw ne sw se) (k+1) bs
goUp (Zipper se k (SECrumb nw ne sw : bs)) = Just $ Zipper (Node nw ne sw se) (k+1) bs

goDn :: Direction -> Zipper a -> Maybe (Zipper a)
goDn _  (Zipper _                  0 _)  = Nothing
goDn _  (Zipper (Leaf _)  _ _)           = Nothing
goDn d  (Zipper Empty k bs)              = goDn d (Zipper (Node Empty Empty Empty Empty) k bs)
goDn NW (Zipper (Node nw ne sw se) k bs) = Just $ Zipper nw (k-1) (NWCrumb ne sw se : bs)
goDn NE (Zipper (Node nw ne sw se) k bs) = Just $ Zipper ne (k-1) (NECrumb nw sw se : bs)
goDn SW (Zipper (Node nw ne sw se) k bs) = Just $ Zipper sw (k-1) (SWCrumb nw ne se : bs)
goDn SE (Zipper (Node nw ne sw se) k bs) = Just $ Zipper se (k-1) (SECrumb nw ne sw : bs)

step :: Direction -> Zipper a -> Maybe (Zipper a)
step N z@(Zipper _ _ (NWCrumb{} : _)) = goUp z >>= goDn SW
step N z@(Zipper _ _ (NECrumb{} : _)) = goUp z >>= goDn SE
step N (Zipper sw k (SWCrumb nw ne se : bs)) = Just $ Zipper nw k (NWCrumb ne sw se : bs)
step N (Zipper se k (SECrumb nw ne sw : bs)) = Just $ Zipper ne k (NECrumb nw sw se : bs)

topmost :: Zipper a -> Zipper a
topmost z = case goUp z of
                Just z' -> topmost z'
                Nothing -> z

pathTo :: Vec2 -> Int -> [Direction]
pathTo _     0 = []
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

empty :: Int -> Quadtree a
empty k = Quadtree k Empty

insert :: Vec2 -> a -> Quadtree a -> Quadtree a
insert pt val qt@(Quadtree k _) =
    Quadtree k $ quad $ topmost (applyByPath insert' path (top qt))
    where   path      = pathTo pt k
            insert' _ = Leaf val

delete :: Vec2 -> Quadtree a -> Quadtree a
delete pt qt@(Quadtree k _) =
    Quadtree k $ quad $ topmost (applyByPath delete' path (top qt))
    where   path      = pathTo pt k
            delete' _ = Empty

insertList :: Quadtree a -> [(Vec2, a)] -> Quadtree a
insertList = foldl (\acc (pt,val) -> insert pt val acc)
