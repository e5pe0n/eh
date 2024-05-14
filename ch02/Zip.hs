module Main where

zipWith' f xs ys = [f (xs !! i) (ys !! i) | i <- [0 .. length xs - 1]]

zipWith'' f xs ys = foldl helper [] [0 .. length xs - 1]
  where
    helper acc i = acc <> [f (xs !! i) (ys !! i)]

main = putStrLn $ show (zipWith' (,) [1 .. 5] [5, 4 .. 1]) <> "\n" <> show (zipWith'' (,) [1 .. 5] [5, 4 .. 1])

-- [(1,5),(2,4),(3,3),(4,2),(5,1)]
-- [(1,5),(2,4),(3,3),(4,2),(5,1)]
