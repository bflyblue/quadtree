module Quadtree
where

import Control.Monad
import Data.Bits

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

data Direction = NW | NE | SW | SE | N | S | W | E | UP
               deriving (Eq, Enum, Show, Bounded)

type Breadcrumbs a = [Crumb a]

data Zipper a = Zipper (Quad a) (Breadcrumbs a)

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
            where   emptynode = Node k' e e e e
                    k'        = k - 1
                    e         = Empty k'
        Zipper (Node k nw ne sw se) bs ->
            case d of
                NW -> Just $ Zipper nw (NWCrumb k' ne sw se : bs)
                NE -> Just $ Zipper ne (NECrumb k' nw sw se : bs)
                SW -> Just $ Zipper sw (SWCrumb k' nw ne se : bs)
                SE -> Just $ Zipper se (SECrumb k' nw ne sw : bs)
            where k' = k - 1

step :: Direction -> Zipper a -> Maybe (Zipper a)
step d zipper =
    let Zipper _ (b:_) = zipper
    in  case d of
            N -> case b of
                NWCrumb{} -> goUp zipper >>= step N >>= goDn SW
                NECrumb{} -> goUp zipper >>= step N >>= goDn SE
                SWCrumb{} -> goUp zipper >>= goDn NW
                SECrumb{} -> goUp zipper >>= goDn NE
            S -> case b of
                NWCrumb{} -> goUp zipper >>= goDn SW
                NECrumb{} -> goUp zipper >>= goDn SE
                SWCrumb{} -> goUp zipper >>= step S >>= goDn NW
                SECrumb{} -> goUp zipper >>= step S >>= goDn NE
            W -> case b of
                NWCrumb{} -> goUp zipper >>= step W >>= goDn NE
                NECrumb{} -> goUp zipper >>= goDn NW
                SWCrumb{} -> goUp zipper >>= step W >>= goDn SE
                SECrumb{} -> goUp zipper >>= goDn SW
            E -> case b of
                NWCrumb{} -> goUp zipper >>= goDn NE
                NECrumb{} -> goUp zipper >>= step E >>= goDn NW
                SWCrumb{} -> goUp zipper >>= goDn SE
                SECrumb{} -> goUp zipper >>= step E >>= goDn SW

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

empty :: Int -> Quad a
empty = Empty

insert :: Vec2 -> a -> Quad a -> Quad a
insert pt val q@(Node k _ _ _ _) =
    quad $ topmost (applyByPath insert' path (top q))
    where   path      = pathTo pt k
            insert' _ = Leaf val

delete :: Vec2 -> Quad a -> Quad a
delete pt q@(Node k _ _ _ _) =
    quad $ topmost (applyByPath delete' path (top q))
    where   path      = pathTo pt k
            delete' _ = Empty k

insertList :: Quad a -> [(Vec2, a)] -> Quad a
insertList = foldl (\acc (pt,val) -> insert pt val acc)
