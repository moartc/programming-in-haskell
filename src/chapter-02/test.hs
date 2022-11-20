double x = x + x

quadruple x = double (double x)

-- Factorial of a positive integer:
factorial n = product[1..n]

-- Average of a list of integers:
average ns = sum ns `div` length ns

{-
  Testing nested
  comment
-}

n = a `div` length xs -- 1. name must begin with a lower-case letter, 2. wrong brackets
  where
    a = 10
    xs = [1,2,3,4,5] -- 3. incorrect indentation


-- Exercise 4
my_last xs = xs !! (length xs - 1)

-- Exercise 5
my_init_1 xs = take (length xs - 1) xs
my_init_2 xs = reverse (tail (reverse xs))
