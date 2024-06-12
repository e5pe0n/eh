{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module HCat where

import qualified Control.Exception as Exception
import qualified Data.ByteString as BS
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Data.Time.Clock as Clock
import qualified Data.Time.Clock.POSIX as PosixClock
import qualified Data.Time.Format as TimeFormat
import qualified System.Directory as Directory
import qualified System.Environment as Env
import System.IO
import qualified System.IO.Error as IOError
import qualified System.Info as SystemInfo
import System.Process (readProcess)
import Text.Printf

runHCat :: IO ()
runHCat = handleIOError $ do
  args <- handleArgs
  targetFilePath <- eitherToErr args
  fileHandle <- openFile targetFilePath ReadMode
  contents <- TextIO.hGetContents fileHandle
  termSize <- getTerminalSize
  hSetBuffering stdout NoBuffering
  finfo <- fileInfo targetFilePath
  let pages = paginate termSize finfo contents
  showPages pages
  where
    handleIOError :: IO () -> IO ()
    handleIOError ioAction = Exception.catch ioAction $ \e -> putStrLn "I ran into an error: " >> print @IOError e

handleArgs :: IO (Either String FilePath)
handleArgs =
  parseArgs <$> Env.getArgs
  where
    parseArgs argumentList =
      case argumentList of
        [fname] -> Right fname
        [] -> Left "no filename provided"
        _ -> Left "multiple files not supported"

eitherToErr :: (Show a) => Either a b -> IO b
eitherToErr (Right a) = return a
eitherToErr (Left e) = Exception.throwIO . IOError.userError $ show e

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

paginate :: ScreenDimensions -> FileInfo -> Text.Text -> [Text.Text]
paginate (ScreenDimensions rows cols) finfo text =
  let rows' = rows - 1
      wrappedLines = concatMap (wordWrap cols) (Text.lines text)
      pages = map (Text.unlines . padTo rows') $ groupsOf rows' wrappedLines
      pageCount = length pages
      statusLines = map (formatFileInfo finfo cols pageCount) [1 .. pageCount]
   in zipWith (<>) pages statusLines
  where
    padTo :: Int -> [Text.Text] -> [Text.Text]
    padTo lineCount rowsToPad =
      take lineCount $ rowsToPad <> repeat " "

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

clearScreen :: IO ()
clearScreen =
  BS.putStr "\^[[1J\^[[1;1H"

showPages :: [Text.Text] -> IO ()
showPages [] = return ()
showPages (page : pages) =
  clearScreen
    >> TextIO.putStr page
    >> getContinue
    >>= \input ->
      case input of
        Continue -> showPages pages
        Cancel -> putStrLn "" >> return ()

data FileInfo = FileInfo
  { filePath :: FilePath,
    fileSize :: Int,
    fileMTime :: Clock.UTCTime,
    fileReadable :: Bool,
    fileWritable :: Bool,
    fileExecutable :: Bool
  }
  deriving (Show)

-- fileInfo :: FilePath -> IO FileInfo
-- fileInfo filePath =
--   Directory.getPermissions filePath >>= \perms ->
--     Directory.getModificationTime filePath >>= \mtime ->
--       BS.readFile filePath >>= \contents ->
--         let size = BS.length contents
--          in return
--               FileInfo
--                 { filePath = filePath,
--                   fileSize = size,
--                   fileMTime = mtime,
--                   fileReadable = Directory.readable perms,
--                   fileWritable = Directory.writable perms,
--                   fileExecutable = Directory.executable perms
--                 }
fileInfo :: FilePath -> IO FileInfo
fileInfo filePath = do
  perms <- Directory.getPermissions filePath
  mtime <- Directory.getModificationTime filePath
  size <- BS.length <$> BS.readFile filePath
  return
    FileInfo
      { filePath = filePath,
        fileSize = size,
        fileMTime = mtime,
        fileReadable = Directory.readable perms,
        fileWritable = Directory.writable perms,
        fileExecutable = Directory.executable perms
      }

formatFileInfo :: FileInfo -> Int -> Int -> Int -> Text.Text
formatFileInfo FileInfo {..} maxWidth totalPages currentPage =
  let timestamp =
        TimeFormat.formatTime TimeFormat.defaultTimeLocale "%F %T" fileMTime
      permissionString =
        [if fileReadable then 'r' else '-', if fileWritable then 'w' else '-', if fileExecutable then 'x' else '-']
      statusLine = Text.pack $ printf "%s | permissions: %s | %d bytes | modified: %s | page: %d of %d" filePath permissionString fileSize timestamp currentPage totalPages
   in invertText (truncateStatus statusLine)
  where
    invertText inputStr =
      let reverseVideo = "\^[[7m"
          resetVideo = "\^[[0m"
       in reverseVideo <> inputStr <> resetVideo
    truncateStatus statusLine
      | maxWidth <= 3 = ""
      | Text.length statusLine > maxWidth =
          Text.take (maxWidth - 3) statusLine <> "..."
      | otherwise = statusLine
