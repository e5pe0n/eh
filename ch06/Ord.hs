module OrdExample where

import Data.Word (Word8)
import Prelude hiding (Ord (..), Ordering (..))

data Ordering = LT | EQ | GT

instance Show Ordering where
  show LT = "LT"
  show EQ = "EQ"
  show GT = "GT"

class (Eq a) => Ord a where
  compare :: a -> a -> Ordering

  (<) :: a -> a -> Bool
  a < b = case compare a b of
    LT -> True
    _ -> False

  (<=) :: a -> a -> Bool
  a <= b = case compare a b of
    GT -> False
    _ -> True

  (>) :: a -> a -> Bool
  a > b = case compare a b of
    GT -> True
    _ -> False

  (>=) :: a -> a -> Bool
  a >= b = case compare a b of
    LT -> False
    _ -> True

  max :: a -> a -> a
  max a b = case compare a b of
    GT -> a
    _ -> b

  min :: a -> a -> a
  min a b = case compare a b of
    LT -> a
    _ -> b

instance Ord Word8 where
  compare a b
    | a == b = EQ
    | a == 0 = LT
    | b == 0 = GT
    | otherwise = compare (a - 1) (b - 1)
