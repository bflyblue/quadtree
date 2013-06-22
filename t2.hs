import Control.Monad
import Quadtree2

main :: IO ()
main = do
    let z = return zipper
        s az (x,y,z) = az >>= to (x,y) >>= set (Leaf z)
        z' = foldl s z [(x,y,'a') | x <- [0..1023], y <- [0..1023]]
        d (Just (Zipper q h _)) = q
    print $ d (z' >>= top)
    --print $ z' >>= top
