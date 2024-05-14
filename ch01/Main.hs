module Main where

pairs as bs =
  let as' = filter (`elem` bs) as
      bs' = filter odd bs
      mkPairs a = map (\b -> (a, b)) bs'
   in concat $ map mkPairs as'

partyBudget isAttending willEat foodCost guests =
  foldl (+) 0 $
    [foodCost food | guest <- map fst guests, food <- map snd guests, willEat guest food, isAttending guest]

pairwiseSum xs ys = map (uncurry (+)) $ zip xs ys

findFirst predicate =
  foldr findHelper []
  where
    findHelper listElement maybeFound
      | predicate listElement = [listElement]
      | otherwise = maybeFound

myFoldr func carryValue lst =
  if null lst
    then carryValue
    else func (head lst) $ myFoldr func carryValue (tail lst)

main = print $ [x | x <- [1 .. 10], y <- [2 .. 12], x == y]
