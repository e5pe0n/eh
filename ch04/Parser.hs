module Main where

data StringParser = StringParser
  { runStringParser :: String -> (String, String)
  }

takeCharacters :: Int -> StringParser
takeCharacters numCharacters = StringParser $ \inputString -> splitAt numCharacters inputString

getNextWord :: StringParser
getNextWord = StringParser $ \someString ->
  case break (== ' ') someString of
    (nextWord, "") -> (nextWord, "")
    (nextWord, rest) -> (nextWord, tail rest)

combineParsers :: StringParser -> StringParser -> StringParser
combineParsers firstParser secondParser = StringParser $ \someString ->
  let (_firstPart, firstResult) = runStringParser firstParser someString
   in runStringParser secondParser firstResult

parseString :: StringParser -> String -> String
parseString parser inputString = fst $ runStringParser parser inputString

main = print ""
