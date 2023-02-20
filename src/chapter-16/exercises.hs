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

Applicative laws:

I.      pure id <*> v = v                            -- Identity
II.     pure f <*> pure x = pure (f x)               -- Homomorphism
III.    u <*> pure y = pure ($ y) <*> u              -- Interchange
IV.     pure (.) <*> u <*> v <*> w = u <*> (v <*> w) -- Composition


instance Applicative Maybe where
    pure x                   = Just x
    (<*>) Nothing  _         = Nothing
    (<*>) (Just f) someMaybe = fmap f someMaybe


additionally Functors laws:

functor laws:
fmap id = id
fmap (g . h) = fmap g . fmap h

Maybe functor:
instance Functor Maybe where
    fmap :: (a -> b) -> Maybe a -> Maybe b
    fmap _ Nothing = Nothing
    fmap g (Just x) = Just (g x)


-- 4 proves for each law with for 2 cases: Nothing and Just x

I law: pure id <*> v = v

Case 1: Nothing
pure id <*> Nothing should be equal: Nothing

pure id <*> Nothing
=   { applying pure }
Just id <*> Nothing
=   { applying <*> (3rd definition) }
fmap id Nothing
=   { applying fmap (functor) }
Nothing


Case 2: Just x
pure id <*> (Just x) should be equal: Just x

pure id <*> Just x
=   { applying pure }
Just id <*> Just x
=   { applying <*> (3rd definition) }
fmap id (Just x)
=   { applying fmap (functor) }
Just (id x)
=   { applying id }
Just x
------------------------------------------------------

II law: pure f <*> pure x = pure (f x)

Case 1: x = Nothing
pure f <*> pure Nothing should be equal: pure (f Nothing)

pure f <*> pure Nothing
=   { applying pure }
Just f <*> Just Nothing
=   { applying <*> }
fmap f (Just Nothing)
=   { applying fmap (Functor 3rd def) }
Just (f  Nothing)
=   { unapplying pure }
pure (f Nothing)



Case 2: Just x
pure f <*> pure (Just x) should be equal: pure (f Just x)

pure f <*> pure (Just x)
=   { applying  pure }
Just f <*> Just (Just x)
=   { applying <*> }
fmap f (Just (Just x))
=   { applying fmap (Functor 3rd def) }
Just (f (Just x))
=   { unapplying pure }
pure (f (Just x))


------------------------------------------------------
III law: u <*> pure y = pure ($ y) <*> u
                        pure (\f -> f y) <*> u (another form)

Case 1: u = Nothing, right side should be equal: pure ($ y) <*> Nothing

Nothing <*> pure y
=   { applying <*> }
Nothing
=   { unapplying fmap }
fmap ($ y) Nothing
=   { unapplying <*> }
(Just ($ y)) <*> Nothing
=   { unapplying pure }
pure ($ y) <*> Nothing



Case 2: u = Just x, right side should be equql: pure ($ y) <*> (Just x)

Just x <*> pure y
=   { applying pure }
(Just x) <*> (Just y)
=   { applying <*> }
fmap x (Just y)
=   { applying fmap }
Just (x y)
=   { using $ operator }
Just (x $ y)
=   { using tihs lambda notation: ($y) = \x -> x $ y }
Just (($y) x)
=   { unapplying fmap from Functor }
fmap ($y) (Just x)
=   { unapplying <*> }
(Just $y) <*> (Just x)
=   { unapplying pure for left side }
pure ($y) <*> (Just x)


------------------------------------------------------
IV law: pure (.) <*> u <*> v <*> w = u <*> (v <*> w)

-- additionally dot operator (.)
(.) :: (b -> c) -> (a -> b) -> a -> c
(.) f g = \x -> f (g x)

Case 1: u = Nothing, right side should be equal: Nothing <*> (v <*> w)

pure (.) <*> Nothing <*> v <*> w
=   { applying pure }
(Just (.)) <*> Nothing <*> v <*> w
=   { applying <*> }
(fmap (.) Nothing) <*> v <*> w
=   { applying fmap }
Nothing <*> v <*> w
=   { applying <*> }
Nothing <*> w
=   { applying <*> }
Nothing
=   { unapplying <*> for the entire right side }
Nothing <*> (v <*> w)


-- Since any occurrence of Nothing makes the whole expression Nothing, case 2 contains only "Just"


Case 2: only Just, right side should be equal: Just u <*> ( Just v <*> Just w)

pure (.) <*> Just u <*> Just v <*> Just w 
=   { applying pure }
(Just (.)) <*> Just u <*> Just v <*> Just w 
=   { applying <*> }
fmap (.) Just u <*> Just v <*> Just w
=   { applying fmap for Functor }
Just ((.) u) <*> Just v <*> Just w
=   { applying <*> }
fmap ((.) u) v <*> Just w
=   { applying fmap }
Just ((((.) u) v) w)
=   { applying (.) operator - 'w' is 'x' from the formula}
Just u (v w)
=   { unapplying fmap }
fmap u (Just (v w))
=   { unapplying <*> }
Just u <*> (Just (v w))
=   { unapplying fmap }
Just u <*> (fmap v (Just w))
=   { unapplying <*> }
Just u <*> (Just v <*> Just w)



-- Exercise 10

Verify the monad laws for the list type. 


Applicative laws:

I.      pure id <*> v = v                            -- Identity
II.     pure f <*> pure x = pure (f x)               -- Homomorphism
III.    u <*> pure y = pure ($ y) <*> u              -- Interchange
IV.     pure (.) <*> u <*> v <*> w = u <*> (v <*> w) -- Composition

instance Applicative [] where
    pure x    = [x]
    fs <*> xs = [f x | f <- fs, x <- xs]

I.      pure id <*> v = v                            -- Identity

pure id <*> v
=   { applying pure }
[id] <*> v
=   { applying <*> - "pure id" corresponds "fs" and "v" to "xs" }
[f x | f <- [id], x <- xs]
=   { just a simpler form }
[id x, x <- xs]
=   { applying id }
[x, x <- xs]
=
[x] = v


II.     pure f <*> pure x = pure (f x)               -- Homomorphism

pure f <*> pure x
=   { applying pure x2 - "f" becomes "fs" and "x" becomes "xs" }
[fs] <*> [xs]
=   { applying <*> }
[f x | f <- fs, x <- xs]
=   { since the list "fs" and "xs" have only 1 element, respectively 'f' and 'x' }
[f x]
=   { unapplying pure }
pure (f x)


III.    u <*> pure y = pure ($ y) <*> u              -- Interchange

u <*> pure y
=   { applying pure }
u <*> [y]
=   { applying <*> }
[u y | u <- us, y <- ys]
=   { since ys contains only one element }
[u y | u <- us]
=   { using ($) operator }
[u $ y | u <- us]
=   { using $ as an infix operator }
[($y) u | u <- us ]
=   { unapplying <$> }
[($y)] <*> u
=   { unapplying pure }
pure ($y) <*> u


IV.     pure (.) <*> u <*> v <*> w = u <*> (v <*> w) -- Composition

-- additional definition: (.) f g = \x -> f (g x)

left side:
pure (.) <*> u <*> v <*> w
=   { applying pure }
[(.)] <*> u <*> v <*> w
=   { applying <*> - "u" becomes "us" }
[(.) u | u <- us ] <*> v <*> w
=   { applying <*> - "v" becomes "vs" }
[q1 v | q1 <-[(.) u | u <- us ], v <- vs] <*> w
=   { applying <*> - "w" becomes "ws" }
[q2 w | q2 <- [q1 v | q1 <-[(.) u | u <- us ], v <- vs], w <- ws]
=   { resolving q2 }
[(.) u v w, ... rest ]
= { applying (.) }
[ u (v w), ... rest ]

right side:

u <*> (v <*> w)
=   { applying <*> for "v" and "w"}
u <*> [v w | v <- [vs], w <- [ws]]
=   { applying <*> }
[u q | u <- us, q <- [v w | v <- [vs], w <- [ws]]]
=   { resolving q }
[ u (v w) | u <- us, q <- [v w | v <- [vs], w <- [ws]]]


left side == right side


-- Exercise 11

-- Based on reverse' example from Book

-- some data, types and function definition for this Exercise: 
data Expr = Val Int | Add Expr Expr deriving (Show)
type Stack = [Int]
type Code = [Op]
data Op = PUSH Int | ADD deriving (Show)

comp :: Expr -> Code
comp (Val n) = [PUSH n]
comp (Add x y) = comp x ++ comp y ++ [ADD]

comp' :: Expr -> [Op] -> [Op]
comp' e c = comp e ++ c

Base case:
1. for e = Val x and c = [] 
comp' (Val x) []
=   { specifiaction of comp' }
comp (Val x) ++ []
=   { applying comp }
[PUSH x] ++ []
=   { applying ++ }
[PUSH x]
-- I think the above "case" is useless here

2. for e = Val x and c != [] -> cs
comp' (Val x) cs
=   { specifiaction of comp }
comp (Val x) ++ cs
=   { applying comp }
[PUSH x] ++ cs
=   { applying ++ }
PUSH x : cs

Inductive case:
1. for e = (Add x y) and c = [] 
comp' (Add x y) []
=   { specifiaction of comp' }
(comp x ++ comp y ++ [ADD]) ++ []
=   { associativity of ++ }
comp x ++ comp y ++ ([ADD] ++ [])
=   { associativity of ++ }
comp x ++ (comp y ++ ([ADD] ++ []))
=   { applying ++ }
comp x ++ (comp y ++ [ADD])
=   { induction hypothesis for comp y ++ [ADD] }
comp x ++ (comp' y [ADD])
=   { induction hypothesis }
comp' x (comp' y [ADD])
-- again, I don't need it

1. for e = (Add x y) and c = cs
comp' (Add x y) cs
=   { specifiaction of comp' }
(comp x ++ comp y ++ [ADD]) ++ cs
=   { associativity of ++ }
comp x ++ comp y ++ ([ADD] ++ cs)
=   { associativity of ++ }
comp x ++ (comp y ++ ([ADD] ++ cs))
=   { applying ++ }
comp x ++ (comp y ++ (ADD : cs))
=   { induction hypothesis for comp y ++ (ADD : cs) }
comp x ++ (comp' y (ADD : cs))
=   { induction hypothesis }
comp' x (comp' y (ADD : cs))

result: comp' x (comp' y (ADD : c))

Recursive definition of comp':

comp' :: Expr -> [Op] -> [Op]
comp' (Val x) c = PUSH x : c
comp' (Add x y) c = comp' x (comp' y (ADD : c))



