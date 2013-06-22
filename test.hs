import Test.QuickCheck
import Quadtree
import Data.List

prop_insertRange a b c d =
    let t = Quadtree 11 Empty
        t' = insertRange 1 ((a,b),(c,d)) t
        v = foldl' (+) 0 [findDefault 0 (x,y) t' | x <- [0..2047], y <- [0..2047]]
    in  v == (abs (a - b) + 1) * (abs (c - d) + 1)

main :: IO ()
main = do
    quickCheck prop_insertRange
