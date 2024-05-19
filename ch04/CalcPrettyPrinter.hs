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

parseExpr :: Expr -> String
parseExpr expr =
  case expr of
    Lit num -> show num
    Add arg1 arg2 -> parseExpr' arg1 <> " + " <> parseExpr' arg2
    Sub arg1 arg2 -> parseExpr' arg1 <> " - " <> parseExpr' arg2
    Mul arg1 arg2 -> parseExpr' arg1 <> " * " <> parseExpr' arg2
    Div arg1 arg2 -> parseExpr' arg1 <> " / " <> parseExpr' arg2
  where
    parseExpr' :: Expr -> String
    parseExpr' expr' =
      case expr' of
        Lit num -> show num
        _ -> "(" <> parseExpr expr' <> ")"

prettyPrint :: Expr -> String
prettyPrint expr =
  case eval expr of
    Left err -> "Error: " <> err
    Right v -> parseExpr expr <> " = " <> show v

-- main = print $ prettyPrint $ Lit 5 `Add` Lit 10

-- "5 + 10 = 15"
-- main = print $ prettyPrint $ Lit 5 `Add` (Lit 10 `Div` Lit 2)

-- "5 + (10 / 2) = 10"

main = print $ prettyPrint $ Lit 14 `Mul` (Lit 5 `Add` (Lit 10 `Div` Lit 2))

-- "14 * (5 + (10 / 2)) = 140"
