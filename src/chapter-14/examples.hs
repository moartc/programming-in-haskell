import Data.Monoid (All (All), Any (Any), Product (Product, getProduct), Sum (..))
import Data.Foldable (Foldable(fold), Foldable(foldr))

-- Monoids

t1 = mconcat [Sum 2, Sum 3, Sum 4] -- Sum {getSum = 9}

t2 = mconcat [Product 2, Product 3, Product 4] -- Product {getProduct = 24}

t3 = mconcat [All True, All True, All True] -- All {getAll = True}

t4 = mconcat [Any False, Any False, Any False] -- Any {getAny = False}

-- Foldables

data Tree a = Leaf a | Node (Tree a) (Tree a)

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

    foldr f v (Leaf x ) = f x v
    foldr f v (Node l r ) = foldr f (foldr f v r) l

    foldl f v (Leaf x ) = f v x
    foldl f v (Node l r) = foldl f (foldl f v l) r

tree :: Tree Int
tree = Node (Node (Leaf 1) (Leaf 2)) (Leaf 3)

t7 = foldr (+) 0 tree -- 6
t8 = foldl (+) 0 tree -- 6
