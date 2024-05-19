module Main where

data BinaryTree a = Leaf | Branch (BinaryTree a) a (BinaryTree a)

showStringTree :: BinaryTree String -> String
showStringTree bt =
  case bt of
    Leaf -> "Leaf"
    Branch left v right ->
      "(Branch " <> showStringTree left <> " " <> v <> " " <> showStringTree right <> ")"

main = print $ showStringTree (Branch (Branch Leaf "b" (Branch Leaf "c" Leaf)) "a" Leaf)
