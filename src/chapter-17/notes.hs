data Expr = Val Int | Add Expr Expr

-- eval :: Expr -> Int                      -- commented out, below is another definition
-- eval (Val n) = n
-- eval (Add x y) = eval x + eval y

-- t1 = eval (Add (Val 1) (Val 2)) -- 3

-- Adding a stack
type Stack = [Int]

-- eval' :: Expr -> Stack -> Stack
-- eval' e s = eval e : s

-- s1 :: Stack
-- s1 = [1,2,3,4]  -- some stack just for test
-- t2 :: Stack
-- t2 = eval' (Add (Val 1) (Val 2)) s1 -- [3,1,2,3,4]

-- another version of eval

-- push :: Int -> Stack -> Stack
-- push n s = n : s

-- add :: Stack -> Stack
-- add (m : n : s) = n + m : s

-- eval' :: Expr -> Stack -> Stack
-- eval' (Val n) s = push n s
-- eval' (Add x y) s = add (eval' y (eval' x s))

-- eval :: Expr -> Int
-- eval e = head (eval' e [])

-- s2 :: Stack
-- s2 = [1, 2, 3, 4] -- the same test for different definition

-- t3 :: Stack
-- t3 = eval' (Add (Val 1) (Val 2)) s2 -- [3,1,2,3,4]

-- Adding continuation

-- type Cont = Stack -> Stack

-- eval'' :: Expr -> Cont -> Cont
-- eval'' e c s = c (eval' e s)

-- eval'' :: Expr -> Cont -> Cont
-- eval'' (Val n) c s = c (push n s)
-- eval'' (Add x y) c s = eval'' x (eval'' y (c . add)) s

-- eval' :: Expr -> Cont
-- eval' e s = eval'' e id s

-- t4 = eval' (Add (Val 1) (Val 2)) [] -- [3]

-- Defunctionalising
-- haltC :: Cont
-- haltC = id

-- pushC :: Int -> Cont -> Cont
-- pushC n c = c . push n

-- addC :: Cont -> Cont
-- addC c = c . add

-- eval' :: Expr -> Cont
-- eval' e = eval'' e haltC

-- eval'' :: Expr -> Cont -> Cont
-- eval'' (Val n) c = pushC n c
-- eval'' (Add x y) c = eval'' x (eval'' y (addC c))

-- t5 = eval' (Add (Val 1) (Val 2)) [] -- [3]

-- t6 = eval' (Add (Val 1) (Val 2)) s2 -- [3,1,2,3,4]

data Code = HALT | PUSH Int Code | ADD Code deriving (Show)

t7 :: Code
t7 = PUSH 1 (PUSH 2 (ADD HALT)) -- PUSH 1 (PUSH 2 (ADD HALT))

exec :: Code -> Stack -> Stack
exec HALT s = s
exec (PUSH n c) s = exec c (n : s)
exec (ADD c) (m : n : s) = exec c (n + m : s)

comp :: Expr -> Code
comp e = comp' e HALT

comp' :: Expr -> Code -> Code
comp' (Val n) c = PUSH n c
comp' (Add x y) c = comp' x (comp' y (ADD c))


-- Expression 
t10 :: Expr
t10 = (Add (Val 1) (Val 2))

-- Code
t8 :: Code
t8 = comp t10   --PUSH 1 (PUSH 2 (ADD HALT))

-- empty stack
st :: Stack
st = []

t9 :: Stack
t9 = exec t8 st -- [3]
