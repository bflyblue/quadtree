module Quadtree
where

import Data.Bits
import qualified Data.List as L

type Vec2 = (Int, Int)

data Quadtree a = Quadtree Int (Quad a)
                deriving (Eq, Show)

data Quad a = Node { nw, ne, sw, se :: Quad a }
            | Leaf a
            | Empty
            deriving (Eq, Show)

data QDir   = NW | NE | SW | SE
            deriving (Eq, Ord, Enum, Show, Bounded)

quadStep :: Quad a -> QDir -> Quad a
quadStep qt NW = nw qt
quadStep qt NE = ne qt
quadStep qt SW = se qt
quadStep qt SE = sw qt

pathTo :: Vec2 -> Int -> [QDir]
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

applyByPath :: (Quad a -> Quad a) -> [QDir] -> Quad a -> Quad a
applyByPath f []          qt    = f qt
applyByPath f p           Empty = applyByPath f p (Node Empty Empty Empty Empty)
applyByPath f (step:path) node  = collapse node'
    where
        collapse n =
            case n of
                (Node Empty Empty Empty Empty)  -> Empty
                _                               -> n
        node' =
            case step of
                NW -> node{ nw = applyByPath f path (nw node) }
                NE -> node{ ne = applyByPath f path (ne node) }
                SW -> node{ sw = applyByPath f path (sw node) }
                SE -> node{ se = applyByPath f path (se node) }

empty :: Int -> Quadtree a
empty k = Quadtree k Empty

insert :: Vec2 -> a -> Quadtree a -> Quadtree a
insert pt val (Quadtree k q) =
    Quadtree k (applyByPath insert' path q)
    where   path      = pathTo pt k
            insert' _ = Leaf val

delete :: Vec2 -> Quadtree a -> Quadtree a
delete pt (Quadtree k q) =
    Quadtree k (applyByPath delete' path q)
    where   path      = pathTo pt k
            delete' _ = Empty

move :: Vec2 -> Vec2 -> a -> Quadtree a -> Quadtree a
move from to val (Quadtree k q) =
    Quadtree k (applyByPath update' cmnpath q)
    where   fpath     = pathTo from k
            tpath     = pathTo to k
            cmnpath   = commonPrefix [fpath, tpath]
            commonPrefix = map head . takeWhile (\(x:xs)-> all (==x) xs) . L.transpose
            l         = length cmnpath
            fpath'    = drop l fpath
            tpath'    = drop l tpath
            update'   = applyByPath insert' tpath' . applyByPath delete' fpath'
            insert' _ = Leaf val
            delete' _ = Empty

insertList :: Quadtree a -> [(Vec2, a)] -> Quadtree a
insertList = foldl (\acc (pt,val) -> insert pt val acc)

