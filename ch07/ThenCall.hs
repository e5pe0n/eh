module ThenCall where

showWithThenCall :: IO ()
showWithThenCall =
  putStrLn "this is just some text"
    >> putStrLn "there are many lines of it"
    >> putStrLn "not one a new function"

main = showWithThenCall
