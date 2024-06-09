module SumArgs where

import System.Environment (getArgs)
import Text.Read (readMaybe)

sumArgs :: [String] -> Maybe Int
sumArgs strArgs =
  let intArgs = mapM readMaybe strArgs
  in fmap sum intArgs

main = sumArgs <$> getArgs >>= print
