module Main where

import Data.Char
import Data.Text as T

countNonPrintableCharacters :: String -> Int
countNonPrintableCharacters =
  Prelude.length . Prelude.filter (not . isPrint)

countNonPrintableCharactersInText :: Text -> Int
countNonPrintableCharactersInText =
  T.length . T.filter (not . isPrint)

countNonPrintableCharactersStringAndText :: String -> (Int, Int)
countNonPrintableCharactersStringAndText input =
  (countNonPrintableCharacters input, countNonPrintableCharactersInText $ pack input)

main :: IO ()
main = print $ countNonPrintableCharactersStringAndText "\v\t\aHello\r\n"
