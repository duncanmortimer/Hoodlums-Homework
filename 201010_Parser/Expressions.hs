module Expressions where

import Text.Printf
import qualified Data.Map as M
import Control.Applicative

data Expr = Term Int | Var String | Op Operator Expr Expr
  deriving (Show, Eq)

data Operator = Sum | Mult | Sub | Div
  deriving (Show, Eq)
           
type Env a = [(String, a)]
type Result = Either Error Int
type Error = String
             
foldExpr :: (Int -> a) -> (Operator -> a -> a -> a) -> (String -> a) -> Expr -> a
foldExpr ft _ _ (Term i) = ft i
foldExpr ft fo fv (Op o u v) = fo o (foldExpr ft fo fv u) (foldExpr ft fo fv v)
foldExpr _ _ fv (Var x) = fv x

eval :: Env Int -> Expr -> Result
eval env = foldExpr Right getOp2 (resolve env)

getOp2 :: Operator -> Result -> Result -> Result
getOp2 o (Right x) (Right y) = Right $ getOp o x y 
getOp2 o (Right x) (Left s) = Left s
getOp2 o (Left s) _ = Left s

getOp :: Operator -> Int -> Int -> Int
getOp o = case o of
  Sum  -> (+)
  Mult -> (*)
  Sub  -> (-)
  Div  -> div

resolve :: Env Int -> String -> Result
resolve e s = maybe (Left ("Variable " ++ s ++ " not bound in expression.")) Right $ lookup s e

printExpr :: Expr -> String
printExpr = foldExpr show (flip (printf "(%s %s %s)") . printOp) id

printOp o = case o of
  Sum  -> "+"
  Mult -> "*"
  Sub  -> "-"
  Div  -> "/"
  
countTerms :: Expr -> Int
countTerms = foldExpr (const 1) (const (+)) (const 1)

