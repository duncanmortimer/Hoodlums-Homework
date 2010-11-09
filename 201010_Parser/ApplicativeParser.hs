module ApplicativeParser ( Parser (..)
                         , fmap, (<$>)
                         , pure, (<*>), (*>), (<*)
                         , empty, (<|>)
                         , withErr, failErr
                         , chainl
                         , dict, sat
                         , item, char, num, word
                         , dropTrailingSpace
                         ) where

import Control.Applicative
import Data.Char
import Prelude hiding (fail)


newtype Parser a = Parser {parse :: String -> Either (Error, String) (a, String)}
type Error = String

-- instances

instance Functor Parser where
  fmap f p = Parser $ \s -> case parse p s of
                              Right (a, s') -> Right (f a, s')
                              Left failure -> Left failure
instance Applicative Parser where
  pure x = Parser $ \s -> Right (x, s)
  pf <*> pa = Parser $ \s -> case parse pf s of
                               Right (f, s') -> case parse pa s' of
                                                  Right (a, s'') -> Right (f a, s'')
                                                  Left failure -> Left failure
                               Left failure -> Left failure

instance Alternative Parser where
  empty = Parser $ \s -> Left ("", s)
  pa <|> pb = Parser $ \s -> case parse pa s of
                               Left e -> parse pb s
                               Right success -> Right success

-- Combinators

chainl :: Parser a -> Parser (a -> a -> a) -> Parser a
pa `chainl` op = Parser $ \s -> case parse pa s of
                                  Right (a, s') -> cl' pa op a s'
                                  Left  failure -> Left failure
    where
      cl' pa op currVal s = case parse ((\o a -> o currVal a) <$> op <*> pa) s of
                              Right (newVal, s') -> cl' pa op newVal s'
                              Left  failure      -> Right (currVal, s)

multiple1 :: Parser a -> Parser [a]
multiple1 pa = (\a as -> a:as) <$> pa <*> multiple pa
              <|> (:[]) <$> pa
              
multiple :: Parser a -> Parser [a]
multiple pa = multiple1 pa <|> pure []


sat :: (a -> Bool) -> Parser a -> Parser a
sat pred pa = let pred' a = if pred a then Just a else Nothing in
              failErr "Error: Predicate not satisfied" $
              pred' <$> pa

fail :: Error -> Parser a
fail err = Parser $ \s -> Left (err, s)

withErr :: Error -> Parser a -> Parser a
withErr err pa = pa <|> fail err

failErr :: Error -> Parser (Maybe a) -> Parser a
failErr err p = Parser $ \s -> case parse p s of
                                 Right (result, s') -> maybe 
                                                        (Left (err, s)) 
                                                        (\x -> Right (x, s'))
                                                        result
                                 Left failure -> Left failure
                                                 
dict tag dict pa = failErr ("Error ("++tag++"): Not in dictionary") $ 
                     (`lookup` dict) <$> pa

-- Concrete parsers

item :: Parser Char
item = Parser $ \s -> case s of
                        (c:s') -> Right (c, s')
                        _ -> Left ("Error: Nothing to parse", [])

digit :: Parser Int
digit = dict "num" digitD item
    where
      digitD = zip ['0'..'9'] [0..9]

num :: Parser Int
num = foldl (\x y -> x*10+y) 0 <$> multiple1 digit

char :: Char -> Parser Char
char c = sat (== c) item
  
word :: Parser String
word = multiple1 allowedChars
    where
      allowedChars = sat (`elem` letters) item
      letters = ['a'..'z']++['A'..'Z']

space :: Parser String
space = (multiple $ sat isSpace item)

dropTrailingSpace :: Parser a -> Parser a
dropTrailingSpace pa = pa <* space