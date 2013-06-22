import Quadtree
import Data.List (foldl')

main :: IO ()
main = do
    let t = Quadtree 12 Empty
        --s az (x,y,z) = insert z (x,y) az
        --c = [(x,y,x*y) | x <- [0..2047], y <- [0..2047]]
        --t' = foldl' s t c
        t' = insertRange 1 ((0,1),(0,0)) t
        v = foldl' (+) 0 [findDefault 0 (x,y) t' | x <- [0..2047], y <- [0..2047]]
        v' = (abs (0 - 0) + 1) * (abs (0 - 1) + 1)

    print v
    print v'
