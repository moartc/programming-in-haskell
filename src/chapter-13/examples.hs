{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import Control.Applicative (Alternative (..))
import Data.Bits (Bits (xor))
import Data.Char
  ( isAlpha,
    isAlphaNum,
    isDigit,
    isLower,
    isSpace,
    isUpper,
    toUpper,
  )

{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Eta reduce" #-}

newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

item :: Parser Char
item =
  P
    ( \inp -> case inp of
        [] -> []
        (x : xs) -> [(x, xs)]
    )

t1 = parse item "" -- []

t2 = parse item "abc" -- [('a',"bc")]

-- Sequencing parses

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap g p =
    P
      ( \inp -> case parse p inp of
          [] -> []
          [(v, out)] -> [(g v, out)]
      )

t3 = parse (fmap toUpper item) "abc" -- [('A',"bc")]

t4 = parse (fmap toUpper item) "" -- []

instance Applicative Parser where
  pure :: a -> Parser a
  pure v = P (\inp -> [(v, inp)])
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pg <*> px =
    P
      ( \inp -> case parse pg inp of
          [] -> []
          [(g, out)] -> parse (fmap g px) out
      )

t5 = parse (pure 1) "abc" -- [(1,"abc")]

three :: Parser (Char, Char)
three = pure g <*> item <*> item <*> item
  where
    g x y z = (x, z)

t6 = parse three "abcdef" --[(('a','c'),"def")]

t7 = parse three "ab" -- []

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f =
    P
      ( \inp -> case parse p inp of
          [] -> []
          [(v, out)] -> parse (f v) out
      )

three2 :: Parser (Char, Char)
three2 = do
  x <- item
  item
  z <- item
  return (x, z)

t8 = parse three "abcdef" --[(('a','c'),"def")]

t9 = parse three "ab" -- []

-- Making choices

instance Alternative Parser where
  empty = P (\inp -> [])
  p <|> q =
    P
      ( \inp -> case parse p inp of
          [] -> parse q inp
          [(v, out)] -> [(v, out)]
      )

t10 = parse empty "abc" -- []

t11 = parse (item <|> return 'd') "abc" -- [('a',"bc")]

t12 = parse (empty <|> return 'd') "abc" -- [('d',"abc")]

-- Derived primitives

sat :: (Char -> Bool) -> Parser Char
sat p = do
  x <- item
  if p x then return x else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

t13 = parse (char 'a') "abc" -- [('a',"bc")]

string :: String -> Parser String
string [] = return []
string (x : xs) = do
  char x
  string xs
  return (x : xs)

t14 = parse (string "abc") "abcdef" -- [("abc","def")]

t15 = parse (string "abc") "abc1234" -- [("abc","1234")]

t16 = parse (many digit) "123abc" -- [("123","abc")]

t17 = parse (many digit) "abc" -- [("","abc")]

t18 = parse (some digit) "abc" -- []

ident :: Parser String
ident = do
  x <- lower
  xs <- many alphanum
  return (x : xs)

nat :: Parser Int
nat = do
  xs <- some digit
  return (read xs)

space :: Parser ()
space = do
  many (sat isSpace)
  return ()

t19 = parse ident "abc def" -- ("abc"," def")]

t20 = parse nat "123 abc" -- [(123," abc")]

t21 = parse space "   abc" -- [((),"abc")]

-- Parser for integer values
int :: Parser Int
int =
  do
    char '-'
    n <- nat
    return (- n)
    <|> nat

t22 = parse int "-123 abc" -- [(-123," abc")]

-- Handling spacing
token :: Parser a -> Parser a
token p = do
  space
  v <- p
  space
  return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

-- parser for a non-empty list of natural numbers that ifnores spacing around tokens
nats :: Parser [Int]
nats = do
  symbol "["
  n <- natural
  ns <-
    many
      ( do
          symbol ","
          natural
      )
  symbol "]"
  return (n : ns)

t23 = parse nats " [1, 2, 3] " -- [([1,2,3],"")]

t24 = parse nats "[1,2,]" -- []
