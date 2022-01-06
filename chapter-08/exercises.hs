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
balanced :: Tree a -> Bool
