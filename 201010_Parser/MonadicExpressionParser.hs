module MonadicExpressionParser (parse, expr ) where

import Expressions
import MonadicParser

-- Any integer is parsed as a term 
term :: Parser Expr
term = dropTrailingSpace $ do {
           as <- multiple1 $ dict (zip ['0'..'9'] [0..9]) item;
           return $ Term $ foldl (\x y -> 10*x + y) 0 as
         }

-- Variable names can only include letters.
var :: Parser Expr
var = dropTrailingSpace $ do {
        as <- multiple1 $ anyof $ map char $ ['a'..'z']++['A'..'Z'];
        return $ Var as
      }

-- Leaf nodes in our expression tree
leaf :: Parser Expr
leaf = term <||> var

-- Operations come in two flavours, depending on priority
op1 :: Parser (Expr -> Expr -> Expr)
op1 = dropTrailingSpace $ dict (zip ['+', '-'] $ map Op [Sum, Sub]) item

op2 :: Parser (Expr -> Expr -> Expr)
op2 = dropTrailingSpace $ dict (zip ['*', '/'] $ map Op [Mult, Div]) item

-- An expression inside brackets
bracketedExpr :: Parser Expr
bracketedExpr = do {
                  char '(';
                  result <- expr;
                  char ')';
                  return result
                }

-- Parse * and / before + and -; all are left-associative
expr :: Parser Expr
expr = (leaf <||> bracketedExpr) `chainl` op2 `chainl` op1