import Test.QuickCheck
import Quadtree

prop_insert1 :: Int -> Bool
prop_insert1 x = insert (0,0) x (empty 1) == Node 1 (Leaf x) (Empty 0) (Empty 0) (Empty 0)

prop_insert2 :: Int -> Bool
prop_insert2 x = insert (0,0) x (empty 2) == Node 2 (Node 1 (Leaf x) (Empty 0) (Empty 0) (Empty 0)) (Empty 1) (Empty 1) (Empty 1)

main :: IO ()
main = do
    quickCheck prop_insert1
    quickCheck prop_insert2
