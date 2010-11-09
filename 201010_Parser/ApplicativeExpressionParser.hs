module ApplicativeExpressionParser (parse, expr) where

import Expressions
import ApplicativeParser

term :: Parser Expr
term = Term <$> dropTrailingSpace num

var :: Parser Expr
var =  Var <$> dropTrailingSpace word

leaf :: Parser Expr
leaf = term <|> var

op1 :: Parser (Expr -> Expr -> Expr)
op1 = dropTrailingSpace $ dict "op1" op1D item
    where
      op1D = zip ['+','-'] $ map Op [Sum, Sub]

op2 :: Parser (Expr -> Expr -> Expr)
op2 = dropTrailingSpace $ dict "op2" op2D item
    where
      op2D = zip ['*','/'] $ map Op [Mult, Div]

bracketedExpr :: Parser Expr
bracketedExpr = (char '(') *> expr <* (char ')')

expr :: Parser Expr
expr = (leaf <|> bracketedExpr) `chainl` op2 `chainl` op1