luhnDouble :: Int -> Int
luhnDouble x = x*2

luhnSubtract :: Int -> Int
luhnSubtract x = if x > 9 then x - 9 else x

dec2int :: [Int] -> Int
dec2int = foldl (\acc x -> x + 10*acc) 0

isDivisible :: Int -> Bool
isDivisible x = (x `mod` 10) == 0

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f1 f2 xs = [if(b `mod` 2 == 0) then f1 (a) else f2 (a) | (a, b) <- zip xs [0..]]

luhn :: [Int] -> Bool
luhn xs = isDivisible(sum (altMap (luhnSubtract) (luhnSubtract . luhnDouble) (reverse xs)))
