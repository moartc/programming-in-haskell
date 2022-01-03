-- exercise 2
all' :: (a -> Bool) -> [a] -> Bool
all' f []     = True
all' f (x:xs) = f x && all' f (xs)

any' :: (a -> Bool) -> [a] -> Bool
any' f []     = False
any' f (x:xs) = f x || any' f (xs)

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f []                     = []
takeWhile' f (x:xs) | f x == False  = []
                    | otherwise = x : takeWhile' f xs

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' f [] = []
dropWhile' f (x:xs) | f x == False = x:xs
                    | otherwise    = dropWhile' f xs


-- exercise 3
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> f x : xs) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldr (\x xs -> if f(x) then x : xs else xs) []

-- exercise 4
dec2int :: [Int] -> Int
dec2int = foldl (\acc x -> x + 10*acc) 0


-- exercise 5
add' :: Int -> (Int -> Int)
add' x y = x + y

product' :: (Int, Int) -> Int
product' (a,b) = a * b

-- use example
curry' :: ((a,b) -> c) -> (a -> b -> c)
curry' f x y = f(x,y)

uncurry' :: (a -> (b -> c)) -> ((a,b) -> c)
uncurry' f(x, y) = f x y

-- exercise 6
unfold p h t x | p x       = []
               | otherwise = h x : unfold p h t (t x)

type Bit = Int
chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

int2bin :: Int -> [Int]
int2bin = unfold (== 0) (`mod` 2) (`div` 2)

chop8' :: [Bit] -> [[Bit]]
chop8' = unfold (\x -> length x == 0) (take 8) (drop 8)

map2 :: (a -> b) -> [a] -> [b]
map2 f = unfold (\x -> length x == 0) (\x -> f (head x)) (drop 1)

iterate' :: (a -> a) -> a -> [a]
iterate' f = unfold (\_ -> False) (\x -> x) (\x -> f(x))

-- exercise 9
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f1 f2 xs = [if(b `mod` 2 == 0) then f1 (a) else f2 (a) | (a, b) <- zip xs [0..]]
