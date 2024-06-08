module NaturalClas where

class (Show n, Eq n) => Natural n where
  add :: n -> n -> n
  multiply :: n -> n -> n
  additiveIdentity :: n
  multiplicativeIdentity :: n

instance Natural Int where
  add = (+)
  multiply = (*)
  additiveIdentity = 0
  multiplicativeIdentity = 1

data Peano = Z | S Peano

toPeano :: Int -> Peano
toPeano 0 = Z
toPeano n = S $ toPeano (n - 1)

fromPeano :: Peano -> Int
fromPeano Z = 0
fromPeano (S n) = 1 + fromPeano n

instance Eq Peano where
  (==) Z Z = True
  (==) (S a) (S b) = a == b
  (==) _ _ = False

instance Show Peano where
  show Z = "Z"
  show (S a) = "(S" <> show a <> ")"

instance Natural Peano where
  add a Z = a
  add a (S b) = add (S a) b
  multiply Z _ = Z
  multiply (S a) b = add b (multiply a b)
  additiveIdentity = Z
  multiplicativeIdentity = S Z
