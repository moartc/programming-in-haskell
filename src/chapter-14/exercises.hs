import Data.Foldable (Foldable (fold, foldMap, foldl, foldr))
import Data.Monoid (Sum)

-- Exercise 1

-- instance (Monoid a, Monoid b) => Monoid (a, b) where
--   mempty :: (a, b)
--   mempty = (mempty, mempty)
--   mappend :: (a, b) -> (a, b) -> (a, b)
--   (x1, y1) `mappend` (x2, y2) = (x1 `mappend` x2, y1 `mappend` y2)

x1 :: (Sum Int, Sum Int)
x1 = (1, 2)

x2 :: (Sum Int, Sum Int)
x2 = (2, 3)

x3 :: (Sum Int, Sum Int)
x3 = (mempty, mempty)

t1 = x1 `mappend` x2 == (fst x1 `mappend` fst x2, snd x1 `mappend` snd x2)

t2 = x1 `mappend` x3 == (fst x1 `mappend` fst x3, snd x1 `mappend` snd x3)

-- Exercise 2

-- instance Monoid (a -> b) where
--     mempty :: (a -> b)
--     mempty = \_ -> mempty
--     mappend :: (a -> b) -> (a -> b) -> (a -> b)
--     f1  `mappend` f2 = \x -> f1 x `mappend` f2 x

-- Exercise 3

-- Foldable definition
-- class Foldable t where
--     fold:: Monoid a => t a -> a
--     foldMap :: Monoid b => (a -> b) -> t a -> b
--     foldr:: (a -> b -> b) -> b -> t a -> b
--     foldl:: (a -> b -> a) -> a -> t b -> a

-- fold :: Monoid a => Maybe a -> a
-- fold Nothing =  mempty
-- fold (Just x) = x

-- foldMap :: Monoid b => (a -> b) -> Maybe a -> b
-- foldMap _ Nothing = mempty
-- foldMap f (Just x) = f x

-- foldr:: (a -> b -> b) -> b -> Maybe a -> b
-- foldr _ x  Nothing = x
-- foldr f b (Just x) = f x b

-- foldl:: (a -> b -> a) -> a -> Maybe b -> a
-- foldl _ x Nothing = x
-- foldl f b (Just x) = f b x

-- Exercise 4

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Show)

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap g Leaf = Leaf
  fmap g (Node l m r) = Node (fmap g l) (g m) (fmap g r)

instance Foldable Tree where
  fold :: Monoid m => Tree m -> m
  fold Leaf = mempty
  fold (Node l m r) = Data.Foldable.fold l `mappend` m `mappend` Data.Foldable.fold r

  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap _ Leaf = mempty
  foldMap f (Node l m r) = (foldMap f l) `mappend` f m `mappend` (foldMap f r)

  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr f v Leaf = v
  foldr f v (Node l m r) = foldr f (foldr f (f m v) r) l

  foldl :: (b -> a -> b) -> b -> Tree a -> b
  foldl f v Leaf = v
  foldl f v (Node l m r) = foldl f (foldl f (f v m) l) r

dec :: Int -> Maybe Int
dec n = if n > 0 then Just (n - 1) else Nothing

instance Traversable Tree where
  traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse g Leaf = pure Leaf
  traverse g (Node l m r) = (pure Node) <*> traverse g l <*> g m <*> traverse g r

tree :: Tree Int
tree = Node Leaf 2 $ Node Leaf 1 Leaf

tree2 :: Tree Int
tree2 = Node Leaf 1 $ Node Leaf 0 Leaf

t3 = traverse dec tree -- Just (Node Leaf 1 (Node Leaf 0 Leaf))

t4 = traverse dec tree2 -- Nothing

-- Exercise 5

tree3 :: Tree String
tree3 = Node Leaf "abcde" $ Node Leaf "abc" Leaf

tree4 :: Tree Int
tree4 = Node Leaf 3 $ Node Leaf 4 Leaf

filterF :: Foldable t => (a -> Bool) -> t a -> [a]
filterF f = foldMap (\q -> if f q then pure q else mempty)

tGrThan3 :: [String]
tGrThan3 = filterF (\x -> length x > 3) tree3 -- ["abcde"]

tEven :: [Int]
tEven = filterF even tree4 -- [4]
