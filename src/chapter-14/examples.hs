import Data.Foldable (Foldable (fold, foldr))
import Data.Monoid (All (All, getAll), Any (Any, getAny), Product (Product, getProduct), Sum (..))

-- Monoids

t1 = mconcat [Sum 2, Sum 3, Sum 4] -- Sum {getSum = 9}

t2 = mconcat [Product 2, Product 3, Product 4] -- Product {getProduct = 24}

t3 = mconcat [All True, All True, All True] -- All {getAll = True}

t4 = mconcat [Any False, Any False, Any False] -- Any {getAny = False}

-- Foldables

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show)

fold :: Monoid a => Tree a -> a
fold (Leaf x) = x
fold (Node l r) = Data.Foldable.fold l `mappend` Data.Foldable.fold r

t5 = getSum (foldMap Sum [1 .. 10]) -- 55

t6 = getProduct (foldMap Product [1 .. 10]) -- 3628800

instance Foldable Tree where
  fold (Leaf x) = x
  fold (Node l r) = Data.Foldable.fold l `mappend` Data.Foldable.fold r

  foldMap f (Leaf x) = f x
  foldMap f (Node l r) = foldMap f l `mappend` foldMap f r

  foldr f v (Leaf x) = f x v
  foldr f v (Node l r) = foldr f (foldr f v r) l

  foldl f v (Leaf x) = f v x
  foldl f v (Node l r) = foldl f (foldl f v l) r

tree :: Tree Int
tree = Node (Node (Leaf 1) (Leaf 2)) (Leaf 3)

t7 = foldr (+) 0 tree -- 6

t8 = foldl (+) 0 tree -- 6

t9 = null [] -- True

t10 = null (Leaf 1) -- False

t11 = length [1 .. 10] -- 10

t12 = length (Node (Leaf 'a') (Leaf 'b')) -- 2

t13 = foldr1 (+) [1 .. 10] -- 55

t14 = foldl1 (+) (Node (Leaf 1) (Leaf 2)) -- 3

average :: Foldable t => t Int -> Int
average ns = sum ns `div` length ns

t15 = average [1 .. 10] -- 5

t16 = average (Node (Leaf 1) (Leaf 3)) -- 2

t17 = and [True, False, True] -- False

t18 = or (Node (Leaf True) (Leaf False)) -- True

t19 = all even [1, 2, 3] -- False

t20 = any even (Node (Leaf 1) (Leaf 2)) -- True

t21 = concat ["ab", "cd", "ef"] -- "abcdef"

t22 = concat (Node (Leaf [1, 2]) (Leaf [3])) -- [1,2,3]

-- Traversables
dec :: Int -> Maybe Int
dec n = if n > 0 then Just (n - 1) else Nothing

t23 = traverse dec [1, 2, 3] -- Just [0,1,2]

t24 = traverse dec [2, 1, 0] -- Nothing

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap g (Leaf x) = Leaf (g x)
  fmap g (Node l r) = Node (fmap g l) (fmap g r)

instance Traversable Tree where -- I need above Functior definition for this Traversable definition
  traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse g (Leaf x) = pure Leaf <*> g x
  traverse g (Node l r) = pure Node <*> traverse g l <*> traverse g r

t25 = traverse dec (Node (Leaf 1) (Leaf 2)) -- Just (Node (Leaf 0) (Leaf 1))

t26 = traverse dec (Node (Leaf 0) (Leaf 1)) -- Nothing

t27 = sequence [Just 1, Just 2, Just 3] -- Just [1,2,3]

t28 = sequenceA [Just 1, Nothing, Just 3] -- Nothing

t29 = sequenceA (Node (Leaf (Just 1)) (Leaf (Just 2))) -- Just (Node (Leaf 1) (Leaf 2))

t30 = sequenceA (Node (Leaf (Just 1)) (Leaf Nothing)) -- Nothing
