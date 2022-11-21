import Control.Monad
import Data.Char

-- ===================  Functors

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show)

instance Functor Tree where
  fmap g (Leaf x) = Leaf (g x)
  fmap g (Node l r) = Node (fmap g l) (fmap g r)

ex1 :: Tree Int
ex1 = fmap length (Leaf "abc")

ex2 :: Tree Bool
ex2 = fmap even (Node (Leaf 1) (Leaf 2))

inc :: Functor f => f Int -> f Int
inc = fmap (+ 1)

ex3 :: [Int]
ex3 = inc [1, 2, 3, 4, 5]

ex4 :: Maybe Int
ex4 = inc (Just 1)

-- =================== Applicatives

ex5 :: Maybe Integer
ex5 = pure (+ 1) <*> Just 1

ex6 :: Maybe Integer
ex6 = pure (+) <*> Just 1 <*> Just 2

ex7 :: Maybe Integer
ex7 = pure (+) <*> Nothing <*> Just 2

ex8 :: [Integer]
ex8 = pure (+ 1) <*> [1, 2, 3]

ex9 :: [Integer]
ex9 = pure (+) <*> [1] <*> [2]

ex10 :: [Integer]
ex10 = pure (*) <*> [1, 2] <*> [3, 4]

prods :: [Int] -> [Int] -> [Int]
prods xs ys = [x * y | x <- xs, y <- ys]

ex11 :: [Int]
ex11 = prods [1, 2] [3, 4]

prods2 :: [Int] -> [Int] -> [Int]
prods2 xs ys = pure (*) <*> xs <*> ys

ex12 :: [Int]
ex12 = prods2 [1, 2] [3, 4]

getChars :: Int -> IO String
getChars n = sequenceA (replicate n getChar)

ex13 :: [Int]
ex13 = (+ 1) <$> [1, 2]

ex14 :: [Integer]
ex14 = (*) <$> [1, 2, 3] <*> [4, 5, 6]

-- =================== Monads

data Expr = Val Int | Div Expr Expr

eval :: Expr -> Int
eval (Val n) = n
eval (Div x y) = eval x `div` eval y

ex15 :: Int
ex15 = eval (Div (Val 1) (Val 0))

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv n m = Just (n `div` m)

eval2 :: Expr -> Maybe Int
eval2 (Val n) = Just n
eval2 (Div x y) = case eval2 x of
  Nothing -> Nothing
  Just n -> case eval2 y of
    Nothing -> Nothing
    Just m -> safediv n m

ex16a :: Maybe Int
ex16a = eval2 (Div (Val 4) (Val 2))

ex16b :: Maybe Int
ex16b = eval2 (Div (Val 1) (Val 0))

eval3 :: Expr -> Maybe Int
eval3 (Val n) = Just n
eval3 (Div x y) =
  eval2 x >>= \n ->
    eval2 y >>= \m ->
      safediv n m

ex17a :: Maybe Int
ex17a = eval3 (Div (Val 4) (Val 2))

ex17b :: Maybe Int
ex17b = eval3 (Div (Val 1) (Val 0))

eval4 :: Expr -> Maybe Int
eval4 (Val n) = Just n
eval4 (Div x y) = do
  n <- eval2 x
  m <- eval2 y
  safediv n m

ex18a :: Maybe Int
ex18a = eval4 (Div (Val 4) (Val 2))

ex18b :: Maybe Int
ex18b = eval4 (Div (Val 1) (Val 0))

pairs :: [a] -> [b] -> [(a, b)]
pairs xs ys = do
  x <- xs
  y <- ys
  return (x, y)

pairs2 :: [a] -> [b] -> [(a, b)]
pairs2 xs ys = xs >>= \x -> ys >>= \y -> return (x, y)

tree :: Tree Char
tree = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')

rlabel :: Tree a -> Int -> (Tree Int, Int)
rlabel (Leaf _) n = (Leaf n, n + 1)
rlabel (Node l r) n = (Node l' r', n'')
  where
    (l', n') = rlabel l n
    (r', n'') = rlabel r n'

ex19 :: Tree Int
ex19 = fst (rlabel tree 0)

conv :: Char -> Maybe Int
conv c
  | isDigit c = Just $ digitToInt c
  | otherwise = Nothing

ex20 :: Maybe [Int]
ex20 = mapM conv "1234"

ex21 :: [[Integer]]
ex21 = filterM (\x -> [True, False]) [1, 2, 3]

join' :: Monad m => m (m a) -> m a
join' mmx = do
  mx <- mmx
  x <- mx
  return x

ex22 :: [Integer]
ex22 = join' [[1, 2], [3, 4], [5, 6]]

ex23 :: Maybe Integer
ex23 = join' (Just (Just 1))

ex24 :: Maybe a
ex24 = join' (Just Nothing)

ex25 :: Maybe a
ex25 = join' Nothing
