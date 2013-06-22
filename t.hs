import Quadtree
import Data.List (foldl')

main :: IO ()
main = do
    let t = Quadtree 2 Empty
        --s az (x,y,z) = insert z (x,y) az
        --c = [(x,y,x*y) | x <- [0..2047], y <- [0..2047]]
        --t' = foldl' s t c
        t' = setRange 1 ((0,0),(0,0)) t
        v = foldl' (+) 0 [find (x,y) t' | x <- [0..2047], y <- [0..2047]]

    print t'
