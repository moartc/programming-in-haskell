{-# LANGUAGE InstanceSigs #-}

-- Exercise 1
data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Show)

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap g Leaf = Leaf
  fmap g (Node a x b) = Node (fmap g a) (g x) (fmap g b)

-- Exercise 2
-- instance Functor ((->) a) where
--   --   fmap :: (x -> y) -> ((->) a x) -> ((->) a y)
--   --   fmap :: (x -> y) -> (a -> x) -> (a -> y)
--   --   fmap f1 f2  = \x -> f1 (f2 x)
--   --   fmap f1 f2  = f1 . f2
--   fmap = (.)

-- instance Functor [] where
--   --   fmap :: (x -> y) -> ((->) a x) -> ((->) a y)
--   --   fmap :: (x -> y) -> (a -> x) -> (a -> y)
--   --   fmap f1 f2  = \x -> f1 (f2 x)
--   --   fmap f1 f2  = f1 . f2
--   fmap = (.)

-- Exercise 3
-- instance Applicative ((->) r) where
--   pure :: a -> f a
--   --   pure :: a -> (r -> a)
--   --   pure a = \_ -> a
--   --   pure a = const a
--   pure = const

--   --   (<*>) :: f (a -> b) -> f a -> f b
--   --   (<*>) :: ((->) r) (a -> b) -> ((->) r) a -> ((->) r) b
--   --   (<*>) :: (->) r (a -> b) -> (->) r a -> (->) r b
--   (<*>) :: (r -> a -> b) -> (r -> a) -> (r -> b)
--   --   (<*>) f g x = f x (g x)
--   --   (<*>) f g = \x -> f x (g x)
--   f <*> g = \x -> f x (g x)

-- Exercise 4
newtype ZipList a = Z [a] deriving (Show)

instance Functor ZipList where
  fmap :: (a -> b) -> ZipList a -> ZipList b
  fmap g (Z xs) = Z $ map g xs

instance Applicative ZipList where
  pure :: a -> ZipList a
  pure x = Z (repeat x)

  (<*>) :: ZipList (a -> b) -> ZipList a -> ZipList b
  (<*>) (Z gs) (Z xs) = Z [g x | (g, x) <- zip gs xs]

z1 :: ZipList Int
z1 = pure 4

t1 :: ZipList Int
t1 = fmap (+ 2) z1 -- Z [6, 6, 6, 6...]

z2a :: ZipList Int
z2a = pure 4

z2b :: ZipList (Int -> Int)
z2b = Z [(+ 2), (* 3), (`div` 4)]

t2 :: ZipList Int
t2 = z2b <*> z2a -- Z [6, 12, 1]

-- Exercise 5

law4Left :: Applicative f => f (a -> b) -> f (c -> a) -> f c -> f b
law4Left x y z = x <*> (y <*> z)

law4Right :: Applicative f => f (a -> b) -> f (c -> a) -> f c -> f b
law4Right x y z = (pure (.) <*> x <*> y) <*> z
-- x :: (a -> b)
-- y :: f (c -> a)
-- z :: f c

-- Exercise 6

newtype Curr a b = C (a -> b)

instance Functor (Curr r) where
  fmap :: (a -> b) -> Curr r a -> Curr r b
  fmap f (C g) = C (f . g)

instance Applicative (Curr r) where
  pure :: a -> Curr r a
  pure r = C (const r)

instance Monad (Curr r) where
  return :: a -> Curr r a
  return r = C (const r)

  (>>=) :: Curr r a -> (a -> Curr r b) -> Curr r b
  (C f) >>= g = C (\x -> extr ((g . f) x) x)
    where
      extr (C h1) = h1

-- Exercise 7

data Expr a = Var a | Val Int | Add (Expr a) (Expr a) deriving (Show)

instance Functor Expr where
  -- (a -> b) -> f a -> f b
  fmap :: (a -> b) -> Expr a -> Expr b
  fmap g (Var a) = Var (g a)
  fmap g (Val a) = Val a
  fmap g (Add e1 e2) = Add (fmap g e1) (fmap g e2)

instance Applicative Expr where
  -- pure :: a -> f a
  pure :: a -> Expr a
  pure a = Var a

  -- (<*>) :: f (a -> b) -> f a -> f b
  (<*>) :: Expr (a -> b) -> Expr a -> Expr b
  _ <*> Val b = Val b -- constant
  Val a <*> b = Val a -- constant
  Var f <*> b = fmap f b
  -- (Add a b) <*> Val c = Val c -- already handled above
  -- (Add a b) <*> Add c d = Add (a <*> c) (b <*> d)   -- -> this would evalueate t11 to = Add (Var 2) (Var 4)
  (Add a b) <*> c = Add (a <*> c) (b <*> c)

t01 = pure (+ 1) <*> Val 12                -- Val 12
t02 = pure (+ 1) <*> Add (Val 1) (Val 2)   -- Add (Val 1) (Val 2)
t03 = pure (+ 1) <*> Add (Var 1) (Var 2)   -- Add (Var 2) (Var 3)
t04 = pure (\x -> "a" ++ [x]) <*> Var 'a'  -- Var "aa"
t05 = Val 12 <*> Var 1                     -- Val 12
t06 = pure(+1) <*> Var 3                   -- Var 4
t07 = pure(+1) <*> Val 3                   -- Val 3
t09 = Add (pure (+1)) (pure (+2)) <*> Var 2 -- Add (Var 3) (Var 4)
t10 = Add (pure (+1)) (pure (+2)) <*> Add (Var 1) (Var 2) -- Add (Add (Var 2) (Var 3)) (Add (Var 3) (Var 4))

-- Exercise 8

type State = Int

newtype ST a = S (State -> (a, State))

app :: ST a -> State -> (a, State)
app (S st) x = st x

instance Functor ST where
  fmap :: (a -> b) -> ST a -> ST b
  fmap g st = do
    a <- st
    let b = g a
    S (\s -> (b, s))

instance Applicative ST where
  pure :: a -> ST a
  pure x = S (\s -> (x, s))
  (<*>) :: ST (a -> b) -> ST a -> ST b
  stf <*> stx = do
    aTob <- stf
    a <- stx
    let b = aTob a
    S (\s -> (b, s))

instance Monad ST where
  (>>=) :: ST a -> (a -> ST b) -> ST b
  st >>= f = S (\s -> let (x, s') = app st s in app (f x) s')

mySt :: ST Int
mySt = S (\x -> (x + 1, x))

myState :: Int
myState = 12

t11 :: (Int, State)
t11 = app mySt myState -- (13, 12)

t12 :: (Int, State)
t12 = app (fmap (+ 31) mySt) myState -- (44, 12)

mySt2 :: ST (Int -> Char)
mySt2 = S (\x -> (const 'c', x))

t13 :: (Char, State)
t13 = app (mySt2 <*> mySt) myState -- ('c', 12)
