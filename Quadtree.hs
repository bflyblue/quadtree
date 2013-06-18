module Quadtree
where

import Control.Monad
import Data.Bits

type Vec2 = (Int, Int)

data Quad a = Node  { level     :: Int
                    , _nw, _ne,
                      _sw, _se  :: Quad a
                    }
            | Empty { level     :: Int
                    }
            | Leaf  { value     :: a
                    }
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

data Zipper a = Zipper  { quad          :: Quad a
                        , breadcrumbs   :: [Crumb a]
                        } deriving (Eq, Show)

type QTrav a = Quad a -> Quad a
type CrumbCons a = Int -> Quad a -> Quad a -> Quad a -> Crumb a

depth :: Quad a -> Int
depth Leaf{}    = 0
depth q         = level q

top :: Quad a -> Zipper a
top q = Zipper q []

adjust :: (Quad a -> Quad a) -> Zipper a -> Zipper a
adjust f (Zipper q bs) = Zipper (f q) bs

emptyexpand :: Quad a -> Quad a
emptyexpand Empty{level=k} = Node k e e e e where e = Empty (k - 1)
emptyexpand q              = q

emptycollapse :: Quad a -> Quad a
emptycollapse q =
    case q of
        Node k a b c d  ->  if all isEmpty [a,b,c,d]
                            then Empty{level=k}
                            else q
        _               -> q
    where   isEmpty Empty{} = True
            isEmpty _       = False

up :: Zipper a -> Maybe (Zipper a)
up zipper =
    case zipper of
        Zipper _  []                            -> Nothing
        Zipper nw' (NWCrumb k ne' sw' se' : bs) -> Just $ Zipper (n k nw' ne' sw' se') bs
        Zipper ne' (NECrumb k nw' sw' se' : bs) -> Just $ Zipper (n k nw' ne' sw' se') bs
        Zipper sw' (SWCrumb k nw' ne' se' : bs) -> Just $ Zipper (n k nw' ne' sw' se') bs
        Zipper se' (SECrumb k nw' ne' sw' : bs) -> Just $ Zipper (n k nw' ne' sw' se') bs
    where
        n k a b c d = emptycollapse (Node k a b c d)

dn :: QTrav a -> CrumbCons a -> QTrav a -> QTrav a -> QTrav a -> Zipper a -> Maybe (Zipper a)
dn q b b1 b2 b3 (Zipper node bs) =
    case k of
        0 -> Nothing
        _ -> Just $ Zipper (q n') (b':bs)
    where   k   = depth node
            n'  = emptyexpand node
            b'  = b k (b1 n') (b2 n') (b3 n')

nw,ne,sw,se :: Zipper a -> Maybe (Zipper a)
nw = dn _nw NWCrumb _ne _sw _se
ne = dn _ne NECrumb _nw _sw _se
sw = dn _sw SWCrumb _nw _ne _se
se = dn _se SECrumb _nw _ne _sw

topmost :: Zipper a -> Zipper a
topmost zipper =
    case up zipper of
        Just z' -> topmost z'
        Nothing -> zipper

to :: Vec2 -> Zipper a -> Maybe (Zipper a)
to pt zipper =
    let t = topmost zipper
        k = (depth.quad) t
    in pathTo pt k t

at :: Vec2 -> (Quad a -> Quad a) -> Zipper a -> Maybe (Zipper a)
at pt f zipper = liftM (adjust f) (to pt zipper)

pathTo :: Vec2 -> Int -> Zipper a -> Maybe (Zipper a)
pathTo _        0 z = return z
pathTo pt@(x,y) k z =
    case p of
        (False, False) -> nw z >>= pathTo pt nk
        (True , False) -> ne z >>= pathTo pt nk
        (False, True ) -> sw z >>= pathTo pt nk
        (True , True ) -> se z >>= pathTo pt nk
    where   nk = k - 1
            x' = testBit x nk
            y' = testBit y nk
            p  = (x',y')

empty :: Int -> Quad a
empty = Empty

modify :: Vec2 -> (Quad a -> Quad a) -> Quad a -> Quad a
modify pt f q =
    case at pt f (top q) of
        Just z' -> root z'
        Nothing -> q
    where root = quad.topmost

insert :: Vec2 -> a -> Quad a -> Quad a
insert pt val = modify pt insert'
    where   insert' _ = Leaf val

delete :: Vec2 -> Quad a -> Quad a
delete pt q = modify pt delete' q
    where   k = depth q
            delete' _ = Empty k

insertList :: Quad a -> [(Vec2, a)] -> Quad a
insertList = foldl (\acc (pt,val) -> insert pt val acc)
