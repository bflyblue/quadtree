import Quadtree
import Data.List (foldl')

main :: IO ()
main = do
    let t = Quadtree 11 Empty
        s az (x,y,z) = insert z (x,y) az
        c = [(x,y,x+y) | x <- [0..1023], y <- [0..1023]]
        t' = foldl' s t c
        -- t' = insertRange 1 ((0,0),(1023,1023)) t
        v = foldl' (+) 0 [findDefault 0 (x,y) t' | x <- [0..1023], y <- [0..1023]]
        -- v' = (abs (1954 - 43) + 1) * (abs (2004 - 27) + 1)

    -- print t'
    print v
    -- print v'
