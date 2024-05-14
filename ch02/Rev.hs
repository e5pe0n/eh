module Main where

revl = foldl f []
  where
    f acc v = v : acc

revr = foldr f []
  where
    f v acc = acc <> [v]

main = putStrLn $ show (revl [0 .. 5]) <> "\n" <> show (revr [0 .. 5])

-- [5,4,3,2,1,0]
-- [5,4,3,2,1,0]
