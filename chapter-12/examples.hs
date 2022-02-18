inc :: Functor f => f Int -> f Int
inc = fmap (+1)

prods :: [Int] -> [Int] -> [Int]
prods xs ys = [x*y | x <- xs, y <- ys]
p1 = prods [1,2] [2,3]

prods' :: [Int] -> [Int] -> [Int]
prods' xs ys = pure (*) <*> xs <*> ys
p2 = prods' [1,2] [2,3]
