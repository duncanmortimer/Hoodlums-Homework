-- Partly based on "Monadic Parsing in Haskell" by Hutton and Meijer, 1998
module MonadicParser ( Parser (..)
              , fmap, (<$>)
              , return, (>>=)
              , pure, (<*>)
              , empty, (<|>)
              , (<||>), multiple, multiple1
              , chainl, chainr
              , sat, anyof, dict
              , item, char, string
              , dropTrailingSpace 
              ) where

import Control.Applicative
import Data.Char

-- Running a parser returns a list of possible interpretations of the inputted string.
-- An empty list indicates failure to parse. 
newtype Parser a = Parser {parse :: String -> [(a, String)]}

-- Make Parser an instance of some useful typclasses
instance Functor Parser where
    fmap f p = Parser $ \s -> [(f x, s') | (x, s') <- parse p s]

instance Monad Parser where
    return x = Parser $ \s -> [(x, s)]
    p >>= f = Parser $ \s -> concat [parse (f x) s' | (x, s') <- parse p s]

instance Applicative Parser where
    pure = return
    pf <*> pa = do {
                  a <- pa;
                  f <- pf;
                  return (f a)
                }

-- NB: different semantics to that used in the Hoodlums meeting in October; here, I've said that p1 <|> p2 applies both p1 and p2, concatenating the results.  This differs from the Hoodlums approach, as if the first parser succeeds, the second parser is still run.  I define another combinator <||> below which follows the semantics from the Hoodlums meeting.              
instance Alternative Parser where
    empty = Parser $ \s -> []
    p1 <|> p2 = Parser $ \s -> parse p1 s ++ parse p2 s
    
-- Additional parser combinators
-- (<||>) has the semantics of the alternative operator defined in the Hoodlums meeting.  It might not be as efficient though?  Perhaps laziness helps here?
(<||>) :: Parser a -> Parser a -> Parser a
p1 <||> p2 = Parser $ \s -> case parse (p1 <|> p2) s of
                              (r:rs) -> [r]
                              _ -> []
                              
-- multiple p: succeeds if p can parse the first zero or more symbols; returns the results of subsequent applications of p in a list.
multiple :: Parser a -> Parser [a]
multiple p = multiple1 p <||> return []

-- multiple1 p: succeeds if p can parse the first one or more symbols; returns the results in a list.
multiple1 :: Parser a -> Parser [a]
multiple1 p = do {
           a <- p;
           as <- multiple p;
           return (a:as)
          }

-- chainl p op: alternately applies p, then op..., applying the operator returned by op in a left-associative manner to the values returned by p, and chaining everything together. 
chainl :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl` op = do {a <- p; rest a}
    where
      rest a = do {
                 f <- op;
                 b <- p;
                 rest (f a b)
               } <||> return a

-- chainr p op: like chainl, but applying the operator in a right-associative manner.
chainr :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainr` op = do {a <- p; f <- op; rest (f a)} <||> p
    where
      rest fa = do {
                 b <- p;
                 f <- op;
                 rest (fa . (f b))
               } <||> do {b <- p; return (fa b)}

-- Some concrete parsers

-- sat pred p: parse using p, but succeed only if the result matches the predicate.
sat :: (a -> Bool) -> Parser a -> Parser a
sat pred p = do {
               a <- p;
               if pred a then
                   return a
               else
                   empty
             }

-- anyof ps: succeed if any of the ps succeeds, and return the result.
anyof :: [Parser a] -> Parser a
anyof = foldl (<||>) empty

-- item: parses any character and returns it as the result.
item :: Parser Char
item = Parser $ \s -> case s of
                        (c:cs) -> [(c, cs)]
                        _ -> []

-- char x: accepts only x, and returns it as the result.
char :: Char -> Parser Char
char x = sat (==x) item

-- dict dict p: parses using p, then uses the result to look up a return value in dict.
dict :: (Eq a) => [(a, b)] -> Parser a -> Parser b
dict dict pa = do {
                  a <- pa;
                  (maybe empty return $ lookup a dict)
                }

-- string s: succeeds if the head of the input matches the s, and returns s.
string :: String -> Parser String
string "" = return ""
string (c:cs) = do {
                   char c;
                   string cs;
                   return (c:cs)
                 }

-- space: parses any spaces, and returns as a string.
space :: Parser String
space = (multiple $ sat isSpace item)

-- dropTrailingSpace p: parse using p, then drop any trailing spaces in the input and return the result of p.
dropTrailingSpace :: Parser a -> Parser a
dropTrailingSpace p = do {a <- p; space; return a}
