{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import Control.Applicative
import Data.Char

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

