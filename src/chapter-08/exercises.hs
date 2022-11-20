-- Exercise 1

data Nat = Zero | Succ Nat
nat2int :: Nat -> Int
nat2int Zero     = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

add :: Nat -> Nat -> Nat
add Zero n     = n
add (Succ m) n = Succ (add m n)

nat2 = int2nat 2
nat3 = int2nat 3
nat4 = int2nat 4
nat5 = int2nat 5
nat6 = int2nat 6

add1 = nat2int (add nat2 nat3) -- 5
add2 = nat2int (add nat3 nat4) -- 7
add3 = nat2int (add nat6 nat5) -- 11

-- my mult definition
mult :: Nat -> Nat -> Nat
mult Zero n     = Zero
mult (Succ m) n = add n (mult m n)

mult1 = nat2int (mult nat2 nat3) -- 6
mult2 = nat2int (mult nat2 nat4) -- 8
mult3 = nat2int (mult nat4 nat5) -- 20
mult4 = nat2int (mult nat6 nat5) -- 30


-- Exercise 2
{-

data Ordering = LT | EQ | GT
compare :: Ord a => a -> a -> Ordering

-}

data Tree a = Leaf a | Node (Tree a) a (Tree a)
t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))


{- old definition:
occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y)                 = x == y
occurs x (Node l y r) | x == y    = True
                      | x < y     = occurs x l
                      | otherwise = occurs x r
-}

-- my definition:
occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y)     = x == y
occurs x (Node l y r) = case compare x y of
                        EQ        -> True
                        LT        -> occurs x l
                        GT -> occurs x r


-- Exercise 3
data Tree2 a = Leaf2 a | Node2 (Tree2 a) (Tree2 a)

numLeaves :: Tree2 a -> Int
numLeaves (Leaf2 _) = 1
numLeaves (Node2 l r) =  numLeaves l + numLeaves r

balancedTree1 = Node2 (Node2 (Leaf2 1) (Leaf2 4)) (Node2 (Leaf2 6) (Leaf2 9))
unbalancedTree1 = Node2 (Node2 (Leaf2 1) (Node2 (Leaf2 12) (Leaf2 14))) (Leaf2 6)

{-
          x
        /  \
       x    x
      | \  / \
     x  1 2   x
    / \      |\
  3   4     5  6
-}
balancedTree2 = Node2 (Node2 (Node2 (Leaf2 3) (Leaf2 4)) (Leaf2 1)) (Node2 (Leaf2 2) (Node2 (Leaf2 5)(Leaf2 6)))

{-
          x
        /  \
       x    x
      | \  / \
     x  1 2   x
    / \      |\
  3   x     5  6
     / \
    7  8
-}
unbalancedTree2 = Node2 (Node2 (Node2 (Leaf2 3) (Node2 (Leaf2 7) (Leaf2 8))) (Leaf2 1)) (Node2 (Leaf2 2) (Node2 (Leaf2 5)(Leaf2 6)))

balanced :: Tree2 a -> Bool
balanced (Leaf2 _) = True
balanced (Node2 l r) = abs (numLeaves l - numLeaves r) <= 1 && balanced l && balanced r

{-
 balanced balancedTree1 - output: True
 balanced balancedTree2 - output: True
 balanced unbalancedTree1 - output: False
 balanced unbalancedTree2 - output: False
-}

-- Exercise 4
split :: [a] -> ([a],[a])
split xs = (take l xs, drop l xs)
            where l = length xs `div` 2

l1 = [1,2,3,4,5,6,7,8]
l2 = [1,2,3,4,5,6,7]

t1 = Node2 (Node2 (Node2 (Leaf2 1) (Leaf2 2)) (Node2 (Leaf2 3)(Leaf2 4))) (Node2 (Node2 (Leaf2 5)(Leaf2 6)) (Node2 (Leaf2 7)(Leaf2 8)))
{-
expecte Tree for l1:
t1 = Node2 Node2 (Node2 ((Leaf2 1)(Leaf2 2)) Node2 ((Leaf2 3)(Leaf2 4))) Node2 (Node2 ((Leaf2 5)(Leaf2 6)) Node2 ((Leaf2 7)(Leaf2 8)))
-}

balance :: [a] -> Tree2 a
balance [x] = Leaf2 x
balance xs   = Node2 (balance(fst splitted)) (balance (snd splitted))
               where splitted = (split xs)

bl1 = balance l1
bl2 = balance l2

bl1balanced = balanced bl1 -- True
bl2balanced = balanced bl2 -- True

{-
> numLeaves bl1
> 8

> numLeaves bl2
< 7
-}

-- Exercise 5
data Expr = Val Int | Add Expr Expr
folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val v) = f v
folde f g (Add x y) = g (folde f g x) (folde f g y)

-- Exercise 6
expr = Add (Add (Val 2) (Val 3)) (Val 4)
expr2 = Add (Add (Val 2) (Val 3)) (Add (Val 2) (Add (Val 2) (Val 3)))

eval :: Expr -> Int
eval xs = folde (\x -> x) (\x y -> x + y) xs

size :: Expr -> Int
size xs = folde (\_ -> 1) (\x y -> x + y) xs
{-
e1 = eval (expr) returns 9
s1 = size (expr) returns 3
e2 = eval (expr2) returns 12
s2 = size (expr2) returns 5
-}

-- Exercise 7
-- commented out because of: "Duplicate instance declaration"
{-
instance Eq a => Eq (Maybe a) where
  Just x == Just y   = x == y
  Nothing == Nothing = True
  _ == _             = False

instance Eq a => Eq [a] where
  [] == []         = True
  (x:xs) == (y:ys) = x == y && xs == ys
  _ == _           = False

-}
