{-# LANGUAGE TypeApplications #-}

module Main where

import Data.IORef

readWriteRef :: IO Int
readWriteRef = do
  myRef <- newIORef @Int 0
  writeIORef myRef 7
  refValue <- readIORef myRef
  pure refValue

main :: IO ()
main = readWriteRef >>= print
