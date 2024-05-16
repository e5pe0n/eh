module Main where

concatMapr' f xss = foldr (\xs acc -> (foldr (\x acc' -> f x : acc') [] xs) <> acc) [] xss

concatMapl' f xss = foldl (\acc xs -> acc <> (foldl (\acc' x -> acc' <> [f x]) [] xs)) [] xss

-- main = print $ concatMapr' (* 2) [[1, 2, 3], [4, 5, 6]]
main = print $ concatMapl' (* 2) [[1, 2, 3], [4, 5, 6]]

-- [2,4,6,8,10,12]
