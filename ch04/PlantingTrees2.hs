module PlantingTrees2 where

data BinaryTree a = Leaf | Branch (BinaryTree a) a (BinaryTree a)

addElementToIntTree :: BinaryTree Int -> Int -> BinaryTree Int
addElementToIntTree bt x =
  case bt of
    Leaf -> Branch Leaf x Leaf
    Branch left v right ->
      if x <= v
        then addElementToIntTree left x
        else addElementToIntTree right x

doesIntExist :: BinaryTree Int -> Int -> Bool
doesIntExist bt x =
  case bt of
    Leaf -> False
    Branch left v right ->
      v == x || doesIntExist left x || doesIntExist right x
