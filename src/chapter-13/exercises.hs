{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import Control.Applicative (Alternative (..))
import Control.Arrow (Arrow (first))
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
import GHC.IO.Handle (hSetEcho)
import GHC.RTS.Flags (ProfFlags (retainerSelector))
import System.IO (stdin)

{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Eta reduce" #-}

-- ========== Basic definitions ==========
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

-- ========== Sequencing parses ==========
instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap g p =
    P
      ( \inp -> case parse p inp of
          [] -> []
          [(v, out)] -> [(g v, out)]
      )

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

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f =
    P
      ( \inp -> case parse p inp of
          [] -> []
          [(v, out)] -> parse (f v) out
      )

-- ========== Making choices ==========
instance Alternative Parser where
  empty = P (\inp -> [])
  p <|> q =
    P
      ( \inp -> case parse p inp of
          [] -> parse q inp
          [(v, out)] -> [(v, out)]
      )

-- ========== Derived primitives ==========
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

string :: String -> Parser String
string [] = return []
string (x : xs) = do
  char x
  string xs
  return (x : xs)

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

-- ========== Parser for integer values ==========
int :: Parser Int
int =
  do
    char '-'
    n <- nat
    return (-n)
    <|> nat

-- ========== Handling spacing ==========
token :: Parser a -> Parser a
token p = do
  space
  v <- p
  space
  return v

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

-- parser for a non-empty list of natural numbers that ignores spacing around tokens
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

-- ========== Arithmetic expressions ==========
expr :: Parser Int
expr = do
  t <- term
  do
    symbol "+"
    e <- expr
    return (t + e)
    <|> return t

term :: Parser Int
term = do
  f <- factor
  do
    symbol "*"
    t <- term
    return (f * t)
    <|> return f

factor :: Parser Int
factor =
  do
    symbol "("
    e <- expr
    symbol ")"
    return e
    <|> natural

-- ========== Calculator ==========
-- q - quit, c - clear the display, d - delete a char
box :: [String]
box =
  [ "+---------------+",
    "|               |",
    "+---+---+---+---+",
    "| q | c | d | = |",
    "+---+---+---+---+",
    "| 1 | 2 | 3 | + |",
    "+---+---+---+---+",
    "| 4 | 5 | 6 | - |",
    "+---+---+---+---+",
    "| 7 | 8 | 9 | * |",
    "+---+---+---+---+",
    "| 0 | ( | ) | / |",
    "+---+---+---+---+"
  ]

buttons :: String
buttons = standard ++ extra
  where
    standard = "qcd=123+456-789*0()/"
    extra = "QCD \ESC\BS\DEL\n"

-- utils from Chapter 10
getCh :: IO Char
getCh = do
  hSetEcho stdin False
  x <- getChar
  hSetEcho stdin True
  return x

type Pos = (Int, Int)

goto :: Pos -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

writeat :: Pos -> String -> IO ()
writeat p xs = do
  goto p
  putStr xs

cls :: IO ()
cls = putStr "\ESC[2j"

-- utls end

showbox :: IO ()
showbox = sequence_ [writeat (1, y) b | (y, b) <- zip [1 ..] box]

display :: [Char] -> IO ()
display xs = do
  writeat (3, 2) (replicate 13 ' ')
  writeat (3, 2) (reverse (take 13 (reverse xs)))

beep :: IO ()
beep = putStr "\BEL"

process :: Char -> String -> IO ()
process c xs
  | c `elem` "qQ\ESC" = quit
  | c `elem` "dD\BS\DEL" = delete xs
  | c `elem` "=\n" = eval xs
  | c `elem` "cC" = clear
  | otherwise = press c xs

quit :: IO ()
quit = goto (1, 14)

delete :: String -> IO ()
delete [] = calc []
delete xs = calc (init xs)

clear :: IO ()
clear = calc []

press :: Char -> String -> IO ()
press c xs = calc (xs ++ [c])

run :: IO ()
run = do
  cls
  showbox
  clear

-- ============= SOLUTIONS =============

-- Exercise 1
comment :: Parser ()
comment = do
  symbol "--"
  many (sat (/= '\n'))
  return ()

--   return ()

tc0 = parse comment "--comment\n" -- [((),"\n")]

tc1 = parse comment "--\n" -- [((),"")]

tc2 = parse comment "-comment\n" -- []

tc3 = parse comment "comment\n" -- []

tc4 = parse comment "--comment" -- [((),"")]

-- Exercise 2
-- Expression: 2+3+4

-- 1.
--          expr
--        /  |  \
--       /   +   \
--     expr     expr
--      |       / | \
--     term    /  +  \
--      |    expr   expr
--    factor  |      |
--      |    term   term
--     nat    |      |
--      |  factor  factor
--      2     |      |
--           nat    nat
--            |      |
--            3      4

-- 2.
--            expr
--          /  |  \
--         /   +   \
--       expr     expr
--      / | \       |
--     /  +  \    term
--   expr   expr    |
--    |      |    factor
--  term   term     |
--    |      |     nat
-- factor  factor   |
--    |      |      4
--   nat    nat
--    |      |
--    2      3

-- Exercise 3

-- Expression: 2+3

--         expr
--       /  |  \
--      /   +   \
--    term     expr
--     |         |
--   factor     term
--     |         |
--    nat      factor
--     |         |
--     2        nat
--               |
--               3

-- Expression: 2*3*4

--         expr
--          |
--         term
--       /  |  \
--      /   *   \
--   factor     term
--     |       / |  \
--    nat     /  *   \
--     |   factor    term
--     2     |        |
--          nat     factor
--           |        |
--           3       nat
--                    |
--                    4

-- Expression: (2+3)+4

--             expr
--           /  |  \
--          /   +   \
--        term     expr
--         |         |
--       factor     term
--      /  |   \     |
--     (  expr  )  factor
--      /  |  \      |
--     /   +   \    nat
--   term     expr   |
--    |        |     4
--  factor    term
--    |        |
--   nat     factor
--    |        |
--    2       nat
--             |
--             3

-- Exercise 4

-- To parse 'expr' the parser first parses 'term' with 'term + expr' and then tries to parse the rest.
-- On failure, the result is discarded and an alternative expression - 'term' - is parsed a second time.
-- When parsing 'term', the first part ('factor') of 'factor * term' is "calcualted" and when the expression is
-- dropped due to missing "*" the alternative (just 'factor') is recalculated and taken as result - the first
-- calculation of 'factor' is wasted and futile in this case.

-- In the optimized version, in the case of 'expr', the term is always counted (because it is always "needed")
-- and in the absence of "+" in the expression, an empty string is taken - unlike the previous version,
-- recalculation of 'term' is not required.

-- Example for the "previous" version and expression "2":
-- 1. parse term + expr
-- 2. parse the term
-- 3. parse factor for the first part (factor * term)
-- 4. nat -> 2
-- 6. we do not find *
-- 7. parse alternative - factor
-- 8. nat -> 2
-- 9. we do not find +
-- 10. parse term
-- 11. parse factor for the first part (factor * term)
-- 12. nat -> 2
-- 13. we do not find *
-- 14. parse alternative - factor
-- 15. nat -> 2

-- Example for the simplified version and expression "2":
-- 1. parse term
-- 2. parse factor
-- 3. nat -> 2
-- 4. empty for 'term' - there is no "* term"
-- 5. empty for 'expr' - there is no "+ expr"

-- Exercise 5

data Expr = Int Int | Mult Expr Expr | Div Expr Expr | Add Expr Expr | Sub Expr Expr deriving (Show)

expr' :: Parser Expr
expr' = do
  t <- term'
  do
    symbol "+"
    Add t <$> expr'
    <|> return t

term' :: Parser Expr
term' = do
  f <- factor'
  do
    symbol "*"
    Mult f <$> term'
    <|> return f

factor' :: Parser Expr
factor' =
  do
    symbol "("
    e <- expr'
    symbol ")"
    return e
    <|> do
      Int <$> natural

t51 = parse expr' "1 + 2" -- [(Add (Int 1) (Int 2),"")]

t52 = parse expr' "1 * 2" -- [(Mult (Int 1) (Int 2),"")]

t53 = parse expr' "1 + 1 * 3" -- [(Add (Int 1) (Mult (Int 1) (Int 3)),"")]

t54 = parse expr' "1 * 2 + 3" -- [(Add (Mult (Int 1) (Int 2)) (Int 3),"")]

-- Exercise 6

expr'' :: Parser Int
expr'' = do
  t <- term''
  do
    symbol "+"
    e <- expr''
    return (t + e)
    <|> do
      symbol "-"
      e <- expr''
      return (t - e)
    <|> return t

term'' :: Parser Int
term'' = do
  f <- exp''
  do
    symbol "*"
    t <- term''
    return (f * t)
    <|> do
      symbol "/"
      t <- term''
      return (f `div` t)
    <|> return f

factor'' :: Parser Int
factor'' =
  do
    symbol "("
    e <- expr''
    symbol ")"
    return e
    <|> integer

t61 = parse expr'' "4 - 2" -- [(2,"")]

t62 = parse expr'' "-12" -- [(-12,"")]

t63 = parse expr'' "4 + (-2)" -- [(2,"")]

t64 = parse expr'' "6 / 2" -- [(3,"")]

t65 = parse expr'' "5 - (2 / 2)" -- [(4,"")]

-- Exercise 7
-- I also edited term`` method from Exercise 6 here

exp'' :: Parser Int
exp'' = do
  f <- factor''
  do
    symbol "^"
    t <- factor''
    return (f ^ t)
    <|> return f

t71 = parse expr'' "2^3*4" -- 32

t72 = parse expr'' "2^3+4" -- 12

t73 = parse expr'' "2^3" -- 8

t74 = parse expr'' "2^(1+2)" -- 8

t75 = parse expr'' "2^1+2" -- 4

-- Exercise 8

-- consider something like that: 1-2-3-4-5 = ((((1-2)-3)-4)-5)

-- b. implementation
sub :: Parser Int
sub = do
  f <- sub
  do
    symbol "-"
    t <- natural
    return (f - t)
    <|> return f

-- c
t82 = parse sub "1-2-3-4-5"

-- The problem: Exception: stack overflow

-- d
sub' :: Parser Int
sub' = do
  f <- natural
  ns <-
    many
      ( do
          symbol "-"
          natural
      )
  return $ foldl (-) f ns

t83 = parse sub' "1-2-3-4-5" -- -13

-- Exercise 9

-- The following functions have been changed:

calc :: [Char] -> IO ()
calc xs = do
  display xs
  c <- getCh
  if c `elem` buttons
    then do
      writeat (0, 15) $ replicate 50 ' ' -- remove previous error message
      process c xs
    else do
      writeat (0, 15) $ "there is no button: '" ++ [c] ++ "'" -- message below calculator instead of beep
      calc xs

eval :: String -> IO ()
eval xs = case parse expr xs of
  [(n, [])] -> do
    calc (show n)
  [(_, err)] -> do
    writeat (0, 15) $ "Error starts here: " ++ err -- message below calculator instead of beep
    calc xs