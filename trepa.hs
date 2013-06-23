{-# LANGUAGE TypeOperators #-}

import Data.Array.Repa (Z(..), (:.)(..), (!))
import qualified Data.Array.Repa as R
-- import Data.List (foldl')

main :: IO ()
main = do
    let t :: R.Array R.U R.DIM2 Int
        t = R.fromListUnboxed (Z :. 1024  :. 1024)
            [x+y | x <- [0..1023], y <- [0..1023]]
        -- s az (x,y,z) = insert z (x,y) az
        -- c = [(x,y,x+y) | x <- [0..1023], y <- [0..1023]]
        -- t' = foldl' s t c
        -- t' = insertRange 1 ((43,27),(1954,2004)) t
        -- v = foldl' (+) 0 [t ! (Z :. x :. y) | x <- [0..1023], y <- [0..1023]]
        -- v' = (abs (1954 - 43) + 1) * (abs (2004 - 27) + 1)
    v <- R.foldAllP (+) 0 t

    -- print t'
    print v
    -- print v'
