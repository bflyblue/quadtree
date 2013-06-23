import Quadtree
import Data.List (foldl')

main :: IO ()
main = do
    let t = Quadtree 11 Empty
        s az (x,y,z) = insert z (x,y) az
        c = [(x,y,1) | x <- [0..1954], y <- [0..2004]]
        --t' = foldl' s t c
        t' = insertRange 1 ((0,0),(1954,2004)) t
        v = foldl' (+) 0 [findDefault 0 (x,y) t' | x <- [0..1], y <- [0..1]]
        v' = (abs (1054 - 0) + 1) * (abs (324 - 0) + 1)

    print v
    print v'
