module Main where

import Expressions
import qualified MonadicExpressionParser as MEP
import qualified ApplicativeExpressionParser as AEP

-- TODO: Remove code duplication.
parseExpr_MEP :: String -> Either String Expr
parseExpr_MEP s = case MEP.parse MEP.expr s of
                (r:rs) -> case r of
                          (result, "") -> Right result
                          (result,s') -> Left $ "Partial success: \n  expression: "++
                                           show result++"\n  remaining string: "++s'
                _ -> Left $ "Unable to parse string '"++s++"': no result"

parseExpr_AEP :: String -> Either String Expr
parseExpr_AEP s = case AEP.parse AEP.expr s of
                    Right (result, []) -> Right result
                    Right (result,s') -> Left $ "Partial success: \n  expression: "
                                               ++ show result 
                                               ++"\n  remaining string: "++s'
                    Left (err, s') -> Left $ err
                                      ++ ": Unable to parse string '"
                                      ++ s ++ "': no result"

main :: IO ()
main = do
  putStrLn "\nEnter string to parse:";
  inp <- getLine;
  
  putStrLn "Monadic Parser:"
  case parseExpr_MEP inp of
    Left error -> putStrLn error
    Right result -> putStrLn $ "\nSuccess!\nResulting expression: " ++ printExpr result

  putStrLn "Applicative Parser:"
  case parseExpr_AEP inp of
    Left error -> putStrLn error
    Right result -> putStrLn $ "\nSuccess!\nResulting expression: " ++ printExpr result