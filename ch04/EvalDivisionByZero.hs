module Main where

import Text.Read (readEither)

data Expr
  = Lit Int
  | Sub Expr Expr
  | Add Expr Expr
  | Mul Expr Expr
  | Div Expr Expr

type ExprVal = Either String Int

eval :: Expr -> ExprVal
eval expr =
  case expr of
    Lit num -> Right num
    Add arg1 arg2 -> eval' (+) arg1 arg2
    Sub arg1 arg2 -> eval' (-) arg1 arg2
    Mul arg1 arg2 -> eval' (*) arg1 arg2
    Div arg1 arg2 -> eval' div arg1 arg2
  where
    eval' :: (Int -> Int -> Int) -> Expr -> Expr -> ExprVal
    eval' operator arg1 arg2 =
      case eval arg1 of
        Left err -> Left err
        Right v1 ->
          case eval arg2 of
            Left err -> Left err
            Right v2 ->
              if v2 == 0
                then Left "division by zero"
                else Right $ operator v1 v2

parse :: String -> Either String Expr
parse str = case parse' (words str) of
  Left err -> Left err
  Right (e, []) -> Right e
  Right (_, rest) -> Left $ "Found extra tokens: " <> unwords rest
  where
    parse' :: [String] -> Either String (Expr, [String])
    parse' [] = Left "unexpected end of expression"
    parse' (token : rest) =
      case token of
        "+" -> parseBinary Add rest
        "*" -> parseBinary Mul rest
        "-" -> parseBinary Sub rest
        "/" -> parseBinary Div rest
        lit -> case readEither lit of
          Left err -> Left err
          Right lit' -> Right (Lit lit', rest)

    parseBinary :: (Expr -> Expr -> Expr) -> [String] -> Either String (Expr, [String])
    parseBinary exprConstructor args =
      case parse' args of
        Left err -> Left err
        Right (firstArg, rest') ->
          case parse' rest' of
            Left err -> Left err
            Right (secondArg, rest'') ->
              Right (exprConstructor firstArg secondArg, rest'')

run :: String -> String
run expr =
  case parse expr of
    Left err -> "Error: " <> err
    Right expr' ->
      case eval expr' of
        Left err -> "Error: " <> err
        Right answer -> "The answer is: " <> show answer

main = print $ run "- 10 + 1 * 2 / 8 0"

-- "Error: division by zero"
