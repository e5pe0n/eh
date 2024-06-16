{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Exception (IOException, handle)
import Control.Monad (join, void, when)
import Data.Foldable (for_)
import Data.IORef (modifyIORef, newIORef, readIORef, writeIORef)
import Data.List (isSuffixOf)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set (empty, insert, member)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Metrics (Metrics, displayMetrics, newMetrics, tickFailure, tickSuccess, timeFunction)
import System.Directory (canonicalizePath, doesDirectoryExist, doesFileExist, listDirectory)
import Text.Printf

dropSuffix :: String -> String -> String
dropSuffix suffix s
  | suffix `isSuffixOf` s = take (length s - length suffix) s
  | otherwise = s

data FileType
  = FileTypeDirectory
  | FileTypeRegularFile
  | FileTypeOther

classifyFile :: FilePath -> IO FileType
classifyFile fname = do
  isDirectory <- doesDirectoryExist fname
  isFile <- doesFileExist fname
  pure $ case (isDirectory, isFile) of
    (True, False) -> FileTypeDirectory
    (False, True) -> FileTypeRegularFile
    _otherwise -> FileTypeOther

naiveTraversal :: FilePath -> (FilePath -> a) -> IO [a]
naiveTraversal rootPath action = do
  classification <- classifyFile rootPath
  case classification of
    FileTypeOther -> pure []
    FileTypeRegularFile -> pure $ [action rootPath]
    FileTypeDirectory -> do
      contents <- map (fixPath rootPath) <$> listDirectory rootPath
      results <- concat <$> getPaths contents
      pure results
  where
    fixPath parent fname = parent <> "/" <> fname
    getPaths = mapM (\path -> naiveTraversal path action)

traverseDirectory :: Metrics -> FilePath -> (FilePath -> IO ()) -> IO ()
traverseDirectory metrics rootPath action = do
  seenRef <- newIORef Set.empty
  let haveSeenDirectory canonicalPath =
        Set.member canonicalPath <$> readIORef seenRef
      addDirectoryToSeen canonicalPath =
        modifyIORef seenRef $ Set.insert canonicalPath
      handler ex = print ex >> tickFailure metrics
      traverseSubdirectory subdirPath = do
        timeFunction metrics "traverseSubdirectory" $ do
          contents <- listDirectory subdirPath
          for_ contents $ \file' ->
            handle @IOException handler $ do
              let file = subdirPath <> "/" <> file'
              canonicalPath <- canonicalizePath file
              classification <- classifyFile canonicalPath
              result <-
                case classification of
                  FileTypeOther -> pure ()
                  FileTypeRegularFile -> action file
                  FileTypeDirectory -> do
                    alreadyProcessed <- haveSeenDirectory file
                    when (not alreadyProcessed) $ do
                      addDirectoryToSeen file
                      traverseSubdirectory file
              tickSuccess metrics
              pure result
  traverseSubdirectory (dropSuffix "/" rootPath)

directorySummaryWithMetrics :: FilePath -> IO ()
directorySummaryWithMetrics root = do
  metrics <- newMetrics
  histogramRef <- newIORef (Map.empty :: Map.Map Char Int)
  traverseDirectory metrics root $ \file -> do
    putStrLn $ file <> " :"
    contents <- timeFunction metrics "TextIO.readFile" $ TextIO.readFile file
    timeFunction metrics "wordcount" $
      let wordCount = length $ Text.words contents
       in putStrLn $ " word count: " <> show wordCount
    timeFunction metrics "histogram" $ do
      oldHistogram <- readIORef histogramRef
      let addCharToHistogram histogram letter =
            Map.insertWith (+) letter 1 histogram
          newHistogram =
            Text.foldl' addCharToHistogram oldHistogram contents
      writeIORef histogramRef newHistogram
  histogram <- readIORef histogramRef
  putStrLn "Histogram Data"
  for_ (Map.toList histogram) $ \(letter, count) ->
    putStrLn $ printf " %c: %d" letter count
  displayMetrics metrics

main :: IO ()
-- main = traverseDirectory' "." id >>= print
main = directorySummaryWithMetrics "."
