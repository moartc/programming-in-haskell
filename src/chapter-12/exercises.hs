{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

-- Exercise 1
data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Show)

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap g Leaf = Leaf
  fmap g (Node a x b) = Node (fmap g a) (g x) (fmap g b)

-- Exercise 2
instance Functor ((->) a) where
  --   fmap :: (x -> y) -> ((->) a x) -> ((->) a y)
  --   fmap :: (x -> y) -> (a -> x) -> (a -> y)
  --   fmap f1 f2  = \x -> f1 (f2 x)
  --   fmap f1 f2  = f1 . f2
  fmap = (.)

-- Exercise 3
instance Applicative ((->) r) where
  pure :: a -> f a
  --   pure :: a -> (r -> a)
  --   pure a = \_ -> a
  --   pure a = const a
  pure = const

  --   (<*>) :: f (a -> b) -> f a -> f b
  --   (<*>) :: ((->) r) (a -> b) -> ((->) r) a -> ((->) r) b
  --   (<*>) :: (->) r (a -> b) -> (->) r a -> (->) r b
  (<*>) :: (r -> a -> b) -> (r -> a) -> (r -> b)
  --   (<*>) f g x = f x (g x)
  --   (<*>) f g = \x -> f x (g x)
  f <*> g = \x -> f x (g x)

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
