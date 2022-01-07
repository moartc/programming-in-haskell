data Expr = Val Int | Add Expr Expr

type Cont = [Op]
data Op = EVAL Expr | ADD Int

eval :: Expr -> Cont -> Int
eval (Val n)   c = exec c n
eval (Add x y) c = eval x (EVAL y : c)

exec :: Cont -> Int -> Int
exec []           n = n
exec (EVAL y : c) n = eval y (ADD n : c)
exec (ADD n: c)   m = exec c (n + m)

value :: Expr -> Int
value e = eval e []

{-
Example of use
> value (Add (Add (Val 2) (Val 3)) (Val 4))
< 9
-}
