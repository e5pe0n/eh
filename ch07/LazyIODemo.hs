module LazyIODemo where

lazyIODemo :: IO ()
lazyIODemo =
  let sayHello :: IO ()
      sayHello = putStrLn "Hello"
      raiseAMathError :: IO Int
      raiseAMathError = putStrLn "I'm part of raiseAMathError" >> return (1 `div` 0)
   in sayHello >> raiseAMathError >> sayHello

main = lazyIODemo

-- Hello
-- I'm part of raiseAMathError
-- Hello
