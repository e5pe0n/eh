{-# LANGUAGE OverloadedStrings #-}

module Main where

import FilePack
import Text.Printf

main :: IO ()
main = print $ runParser parseCount "aaaaabbbbbbb"
