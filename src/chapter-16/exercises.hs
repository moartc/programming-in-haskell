-- Exercise 1

add n (Succ m) = Succ (add n m)

Base case (n = Zero):

add Zero (Succ m)
=   { applying add }
Succ m
=   { unapplying add }
Succ (add Zero m)

Inductive case (n = Succ n):

add (Succ n) (Succ m)
=   { applying add }
Succ (add n (Succ m))
=   { inducative hypothesis }
Succ (Succ (add n m))
=   { unapplying add}
Succ (add (Succ n) m)


-- Exercise 2

proerty to use: add n Zero = n, 
 
show that addition is commutative, add n m = add m n, by induction on n.

Base case (n = Zero):

add Zero m
=   { applying add }
m 
=   { using property }
add m Zero

Inductive case (n = Succ n):

add (Succ n) m 
=   { applying add }
Succ (add n m)
=   { induction hypothesis }
Succ (add m n)
=   { using property from exercise 1 }
add m (Succ n)


-- Exercise 3

all p []  = True
all p (x:xs) = p x && all p xs

Base case (for replicate 0 x):
all (== x) (replicate 0 x)
=   { applying replicate }
all (== x) []
=   { applying all }
True

Inductive case: (for replicate (n+1) x)

all (== x) (replicate (n+1) x)
=    {applying replicate }
all (== x) (x : replicate n x)
=    { applying all }
x == x && all (==x) (replicate n x)
=   { applying (==) }
True && all (== x) (replicate n x)
=   { applying && }
all (== x) (replicate n x)
=   { induction hypothesis }
True


-- Exercise 4

definition:
[] ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

1. xs ++ [] = xs

Base case (xs = []):
[] ++ []
=   { using definition #1 }
= []

Inductive case (xs = (x:xs))
(x:xs) ++ []
=   { using defnition #2 }
x : (xs ++ [])
=   { induction hypothesis }
= x : xs


2. xs ++ (ys ++ zs) = (xs ++ ys) ++ zs

Base case (xs = []):
[] ++ (ys ++ zs)
=   { using definition #1 }
ys ++ zs
=   { unapplying definition #1 }
([] ++ ys) ++ zs

Inductive case (xs = (x:xs)) -> (x:xs) ++ (ys ++ zs) should be equals ((x:xs) ++ ys) ++ zs
(x:xs) ++ (ys ++ zs)
=   { using definition #2 }
x : (xs ++ (ys ++ zs))
=   { induction hyphothesis }
x : ((xs ++ ys) ++ zs)
=   { using definition #2 }
((x:xs) ++ ys) ++ zs



-- Exercise 5

5. Using the above deﬁnition for ++, together with

take 0 _    = []
take _ []   = []
take n (x:xs) = x : take (n-1) xs

drop 0 xs   = xs
drop _ []   = []
drop n (_:xs) = drop (n-1) xs

show that `take n xs ++ drop n xs = xs`, by simultaneous induction on the integer n >= 0 and the list xs. 

take n xs ++ drop n xs = xs

Case 1: n = (n+1), xs = [] -> right side should be = xs -> []

take (n+1) [] ++ drop (n+1) []
=   { using definition #2 from take and #2 from drop }
[] ++ []
=   { using definition from Exercise 4 }
[]

Case 2: n = 0, xs = (x:xs) -> right should be (x:xs)

take 0 (x:xs) ++ drop 0 (x:xs)
=   { using definition #1 for take and drop }
[] ++ (x:xs)
=   { using definition from Exercise 4 }
(x:xs)

Case 3: n = (n+1), xs = (x:xs) -> right should be (x:xs)
take (n+1) (x:xs) ++ drop (n+1) (x:xs)
=   { applying definition #3 from take }
x : take n xs ++ drop (n+1) (x:xs)
=   { applying definition #3 from drop }
x : take n xs ++ drop n xs
=   { induction hypothesis }
x : xs


-- Exercise 6

data Tree = Leaf Int | Node Tree Tree

Definitions:

countLeaves :: Tree -> Int
countLeaves (Leaf _) = 1
countLeaves (Node l r) = countLeaves l + countLeaves r

countNodes :: Tree -> Int
countNodes (Leaf _) = 0
countNodes (Node l r) = 1 + countNodes l + countNodes r

To show: countLeaves - 1 = countNodes


Base case (for Leaf) -> left should be equal: countNodes (Leaf _)
countLeaves (Leaf) - 1
=   { applying countLeaves }
1 - 1  
=   { applying - }
0
=   { unapplying countNodes }
countNodes (Leaf _)

Inductive case (for Node l r) -> right should be equal: `countNodes (Node l r)`

countLeaves (Node l r) - 1
=   { applying countLeaves }
(countLeaves l + countLeaves r) - 1
=   { applying hypothesis - countLeaves = countNodes + 1}
(coundNodes l + 1 + coundNodes r + 1) - 1
=   { reordering and applying - }
1 + coundNodes l + coundNodes r
=   { unapplying definition countNodes }
countNodes (Node l r)


-- Exercise 7

Verify the functor laws for the Maybe type. 
Hint: the proofs proceed by case analysis, and do not require the use of induction.

functor laws:
fmap id = id
fmap (g . h) = fmap g . fmap h

Maybe functor:
instance Functor Maybe where
fmap :: (a -> b) -> Maybe a -> Maybe b
fmap _ Nothing = Nothing
fmap g (Just x) = Just (g x)

1. fmap id = id

Case 1: Nothing
fmap id Nothing should be equal: id Nothing

fmap id Nothing
=   { applying fmap } 
Noting
=   { unapplying id }
= id Nothing

Case 2: Just x
fmap id (Just x) should be equals: id Just x

fmap id (just x)
=   { applying fmap }
Just (id x)
=   { applying id }
Just (x)
=   { unapplying id }
id (Just x)

2. fmap (g . h) = fmap g . fmap h

Case 1: Nothing
fmap (g . h) Nothing
=   { applying fmap }
Nothing
=   { unapplying fmap (for h) }
fmap h Nothing
=   { unapplying fmap (for g) }
fmap g (fmap h Nothing)
=   { using  composition }
fmap g . h Nothing

Case 2: Just x
fmap (g . h) Just x
=   { applying fmap }
Just ((g . h) x)
=   { applying composition }
Just (g (h x))
=   { unapplying outer map }
fmap g Just (h x)
=   { unapplying inner map }
fmap g (fmap h (Just x))
=   { unapplying composition }
(fmap g) . (fmap h) (Just x)


-- Exercise 8

Verify the functor laws for the Tree type, by induction on trees.

data Tree a = Leaf a | Node (Tree a) (Tree a)
instance Functor Tree where
-- fmap :: (a -> b) -> Tree a -> Tree b
fmap g (Leaf x) = Leaf (g x)
fmap g (Node l r) = Node (fmap g l) (fmap g r)

1. fmap id = id

Case 1: Leaf x
fmap id (Leaf x) should be equal: id (Leaf x)

fmap id (Leaf x)
=   { applying fmap }
Leaf (id x)
=   { applying id }
Leaf x
=   { unapplying id }
id (Leaf x)


Case 2: Node l r
fmap id (Node l r) should be equals: id (Node l r)

fmap id (Node l r)
=   { applying fmap }
Node (fmap id l) (fmap id r)
=   { applying id x2 }
Node (l r)
=   { unapplying id }
id (Node l r)


2. fmap (g . h) = fmap g . fmap h
Case 1: Leaf x
fmap (g . h) (Leaf x) should be equal: fmap g . fmap h (Leaf x)

fmap (g . h) (Leaf x)
=   { applying fmap }
Leaf ((g . h) x)
=   { applying composition }
Leaf (g (h x))
=   { unapplying outer map }
fmap g (Leaf (h x))
=   { unapplying inner map }
fmap g (fmap h (Leaf x))
=   { unapplying composition }
fmap g . fmap h (Leaf x)

Case 2: Node l r
fmap (g . h) (Node l r) should be equal: fmap g . fmap h (Node l r)

fmap (g . h) (Node l r)
=   { applying map }
Node (fmap (g . h) l) (fmap (g . h) r)
=   { applying induction hypothesis for l }
Node ((fmap g . fmap h) l) (fmap (g . h) r)
=   { applying induction hypothesis for r }
Node ((fmap g . fmap h) l) ((fmap g . fmap h) r)
=   { unapplying fmap for g }
fmap g (Node (fmap h l) (fmap h r))
=   { unapplying fmap for h }
fmap g (fmap h (Node l r))
=   { unapplying composition }
fmap g . fmap h (Node l r)


-- Exercise 9
-- Verify the applicative laws for the Maybe type.

Applicative law for Maybe:
-- to do 
