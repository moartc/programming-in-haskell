data Expr = Val Int | Add Expr Expr | Mult Expr Expr
type Cont = [Op]
data Op = EVAL Expr | ADD Int | EVAL2 Expr | MULT Int

eval :: Expr -> Cont -> Int
eval (Val n)   c  = exec c n
eval (Add x y) c  = eval x (EVAL y : c)
eval (Mult x y) c = eval x (EVAL2 y : c)

exec :: Cont -> Int -> Int
exec []           n  = n
exec (EVAL y : c) n  = eval y (ADD n : c)
exec (EVAL2 y : c) n = eval y (MULT n : c)
exec (ADD n: c) m    = exec c (n + m)
exec (MULT n: c) m   = exec c (n * m)

value :: Expr -> Int
value e = eval e []

-- (2 + 3) + 4 = 9
r0 = value (Add (Add (Val 2) (Val 3)) (Val 4))
-- (2 + 3) * 4 = 20
r1 = value (Mult (Add (Val 2) (Val 3)) (Val 4))
-- (2 * 3) + 4 = 10
r2 = value (Add (Mult (Val 2) (Val 3)) (Val 4))
-- (2 * 3) * 4 = 24
r3 = value (Mult (Mult (Val 2) (Val 3)) (Val 4))
{-
  to test:
  value (r0)
  value (r1)
  value (r2)
  value (r3)
-}
