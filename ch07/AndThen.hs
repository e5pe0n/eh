module AndThen where

import System.IO

doSomeFileStuff =
  openFile "./ReadFile.hs" ReadMode
    >>= \handle ->
      hGetContents handle
        >>= \contents ->
          putStrLn contents
            >>= \_ -> hClose handle

main = doSomeFileStuff
