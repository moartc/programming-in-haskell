-- doesn't terminate
inf :: Int
inf = inf + 1

t1 = fst (0, inf) -- 0

--
ones :: [Int]
ones = 1 : ones

t2 = head ones -- 1

t3 = take 3 ones -- [1,1,1]

t4 = filter (<= 5) [1 ..] -- [1,2,3,4,5 and will loop forever

t5 = takeWhile (<= 5) [1 ..] -- [1,2,3,4,5]

--
primes :: [Int]
primes = sieve [2 ..]

sieve :: [Int] -> [Int]
sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

t6 = take 10 primes -- [2,3,5,7,11,13,17,19,23,29]
t7 = takeWhile (<10) primes -- [2,3,5,7]