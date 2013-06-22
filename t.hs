import Quadtree
import Data.List (foldl')

main :: IO ()
main = do
    let t = Quadtree 10 Empty
        s az (x,y,z) = insert z (x,y) az
        t' = foldl' s t [(x,y,x*y) | x <- [0..2047], y <- [0..2047]]
        v = foldl' (+) 0 [Quadtree.lookup (x,y) t' | x <- [0..2047], y <- [0..2047]]

    print v
