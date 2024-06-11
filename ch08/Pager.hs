module Pager where

import Data.Text qualified as Text
import System.IO
import System.Info qualified as SystemInfo
import System.Process (readProcess)

groupsOf :: Int -> [a] -> [[a]]
groupsOf n [] = []
groupsOf n elems =
  let (hd, tl) = splitAt n elems
   in hd : groupsOf n tl

wordWrap :: Int -> Text.Text -> [Text.Text]
wordWrap lineLength lineText
  | Text.length lineText <= lineLength = [lineText]
  | otherwise =
      let (candidate, nextLines) = Text.splitAt lineLength lineText
          (firstLine, overflow) = softWrap candidate (Text.length candidate - 1)
       in firstLine : wordWrap lineLength (overflow <> nextLines)
  where
    softWrap hardwrappedText textIndex
      | textIndex <= 0 = (hardwrappedText, Text.empty)
      | Text.index hardwrappedText textIndex == ' ' =
          let (wrappedLine, rest) = Text.splitAt textIndex hardwrappedText
           in (wrappedLine, Text.tail rest)
      | otherwise = softWrap hardwrappedText (textIndex - 1)

data ScreenDimensions = ScreenDimensions
  { screenRows :: Int,
    screenColumns :: Int
  }
  deriving (Show)

paginate :: ScreenDimensions -> Text.Text -> [Text.Text]
paginate (ScreenDimensions rows cols) text =
  let unwrappedLines = Text.lines text
      wrappedLines = concatMap (wordWrap cols) unwrappedLines
      pageLines = groupsOf rows wrappedLines
   in map Text.unlines pageLines

getTerminalSize :: IO ScreenDimensions
getTerminalSize =
  case SystemInfo.os of
    "darwin" -> tputScreenDimensions
    "linux" -> tputScreenDimensions
    _other -> pure $ ScreenDimensions 25 80
  where
    tputScreenDimensions :: IO ScreenDimensions
    tputScreenDimensions =
      readProcess "tput" ["lines"] "" >>= \lines ->
        readProcess "tput" ["cols"] "" >>= \cols ->
          let lines' = read $ init lines
              cols' = read $ init cols
           in return $ ScreenDimensions lines' cols'

data ContinueCancel = Continue | Cancel deriving (Eq, Show)

getContinue :: IO ContinueCancel
getContinue =
  hSetBuffering stdin NoBuffering
    >> hSetEcho stdin False
    >> hGetChar stdin
    >>= \input ->
      case input of
        ' ' -> return Continue
        'q' -> return Cancel
        _ -> getContinue

runHCat :: IO ()
runHCat =
  putStrLn "do you wan to Continue (space) or quit (q)"
    >> getContinue
    >>= \cont -> case cont of
      Continue -> putStrLn "okay, continuing!" >> runHCat
      Cancel -> putStrLn "goodbye!"

-- main = print $ wordWrap 6 $ Text.pack "word wrapping is tricky"
main = runHCat
