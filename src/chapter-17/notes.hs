data Expr = Val Int | Add Expr Expr

-- eval :: Expr -> Int                      -- commented out, below is another definition
-- eval (Val n) = n                     
-- eval (Add x y) = eval x + eval y

t1 = eval (Add (Val 1) (Val 2)) -- 3

-- Adding a stack
type Stack = [Int]

-- eval' :: Expr -> Stack -> Stack
-- eval' e s = eval e : s

-- s1 :: Stack
-- s1 = [1,2,3,4]  -- some stack just for test
-- t2 :: Stack
-- t2 = eval' (Add (Val 1) (Val 2)) s1 -- [3,1,2,3,4]

-- another version of eval

push :: Int -> Stack -> Stack
push n s = n : s

add :: Stack -> Stack
add (m : n : s) = n + m : s

eval' :: Expr -> Stack -> Stack
eval' (Val n) s = push n s
eval' (Add x y) s = add (eval' y (eval' x s))

eval :: Expr -> Int
eval e = head (eval' e [])

s2 :: Stack
s2 = [1,2,3,4]  -- the same test for different definition
t3 :: Stack
t3 = eval' (Add (Val 1) (Val 2)) s2 -- [3,1,2,3,4]