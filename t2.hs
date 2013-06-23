import Quadtree2
import Data.List (foldl')
import Data.Maybe

main :: IO ()
main = do
    let t = return zipper
        s az (x,y,z) = az >>= to (x,y) >>= set (Leaf z)
        g az (x,y) = getDefault 0 $ fromJust $ az >>= to (x,y)
        t' = foldl' s t [(x,y,x+y) | x <- [0..1023], y <- [0..1023]]
        t'' = t >>= top
        v = foldl' (+) 0 [g t' (x,y) | x <- [0..1023], y <- [0..1023]]
        d (Just (Zipper q _ _)) = q
    print $ v
    --print $ z' >>= top
