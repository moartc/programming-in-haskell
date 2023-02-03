-- Exercise 1

-- 1 + (2*3)
-- (2*3) - innermost - it contains no other redex, and outermost - it's contained in no other redex.

-- (1+2) * (2+3)
-- (1+2) - innermost
-- (2+3) - innermost

-- fst (1+2, 2+3)
-- 1+2 - innermost
-- 2+3 - innermost
-- fst (1+2, 2+3) - outermost

-- (\x -> 1 + x) (2*3)
-- (2*3) - innermost
-- (\x -> 1 + x) (2*3) - outermost and innermost

-- Exercise 2

-- outermost:
--     fst (1+2,2+3)
-- = { applying fst }
--     1+2
-- = { applying + }
--     3

-- innermost:
--     fst (1+2,2+3)
-- = { applying the first + }
--     fst (3,2+3)
-- = { applying the second + }
--     fst (3, 5)
-- = { applying fst }
--     3

-- 1 more step for innermost evaluation

-- Exercise 3

-- mult = \x -> (\y -> x * y) (3 4)

--     mult (3 4)
-- = { applying mult }
--     (\x -> (\y -> x * y)) 3 4
-- = { applying outermost lambda }
--     (\y -> 3 * y) 4
-- = { applying the lambda }
--     3 * 4
-- = { applying * }
--     12

-- Exercise 4
fibs :: [Integer]
fibs = 0 : 1 : [x + y | (x, y) <- zip fibs (tail fibs)]

t1 :: [Integer]
t1 = take 10 fibs -- [0,1,1,2,3,5,8,13,21,34]

-- Exercise 5

-- repeat :: a -> [a]
-- repeat x = xs where xs = x:xs

-- take :: Int -> [a] -> [a]
-- take 0 _ = []
-- take _ [] = []
-- take n (x:xs) = x : take (n-1) xs

-- replicate :: Int -> a -> [a]
-- replicate n = take n . repeat

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Show)

-- repeat - generate next 'level' in a tree
repeatT :: a -> Tree a
repeatT x = Node (repeatT x) x (repeatT x)

takeT :: Int -> Tree a -> Tree a
takeT 0 _ = Leaf
takeT _ Leaf = Leaf
takeT n (Node l m r) = Node (takeT (n - 1) l) m (takeT (n - 1) r)

t2 :: Tree Integer
t2 = takeT 0 $ repeatT 3 -- Leaf

t3 :: Tree Integer
t3 = takeT 1 $ repeatT 3 -- Node Leaf 3 Leaf

t4 :: Tree Integer
t4 = takeT 2 $ repeatT 3 -- Node (Node Leaf 3 Leaf) 3 (Node Leaf 3 Leaf)

t5 :: Tree Integer
t5 = takeT 3 $ repeatT 3 -- Node (Node (Node Leaf 3 Leaf) 3 (Node Leaf 3 Leaf)) 3 (Node (Node Leaf 3 Leaf) 3 (Node Leaf 3 Leaf))

replicateT :: a -> Int -> Tree a
replicateT x n = takeT n $ repeatT x

-- replicate Tree with value 12 (Int) 2 times
t6 :: Tree Integer
t6 = replicateT 12 2 -- Node (Node Leaf 12 Leaf) 12 (Node Leaf 12 Leaf)

-- Exercise 6

infAppr :: Double -> [Double]
infAppr n = iterate (next n) 1.0
  where
    next n x = (x + n / x) / 2.0

sqroot :: Double -> Double
sqroot n = helper $ infAppr n
  where
    helper (x : y : xs)
      | x - y < 0.00001 = y
      | otherwise = helper $ y : xs

t7 :: Double
t7 = sqroot 9 -- 3.0

t8 :: Double
t8 = sqroot 10 -- 3.1622776601683795