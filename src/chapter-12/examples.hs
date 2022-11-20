data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show)

instance Functor Tree where
  fmap g (Leaf x) = Leaf (g x)
  fmap g (Node l r) = Node (fmap g l) (fmap g r)

ex1 :: Tree Int
ex1 = fmap length (Leaf "abc")

ex2 :: Tree Bool
ex2 = fmap even (Node (Leaf 1) (Leaf 2))

inc :: Functor f => f Int -> f Int
inc = fmap (+ 1)

ex3 = inc [1, 2, 3, 4, 5] 
ex4 = inc (Just 1)

