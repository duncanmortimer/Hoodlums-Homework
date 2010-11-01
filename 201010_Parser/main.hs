module Main where

import Expressions
import Parser

parseExpr :: String -> Either String Expr
parseExpr s = case parse expr s of
                (r:rs) -> case r of
                          (result, "") -> Right result
                          (result,s') -> Left $ "Partial success: \n  expression: "++
                                           show result++"\n  remaining string: "++s'
                _ -> Left $ "Unable to parse string '"++s++"': no result"

main :: IO ()
main = do
  putStrLn "\nEnter string to parse:";
  inp <- getLine;
  case parseExpr inp of
    Left error -> putStrLn error
    Right result -> putStrLn $ "\nSuccess!\nResulting expression: " ++ printExpr result
    
