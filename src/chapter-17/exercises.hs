-- Exercise 1 - Solution from paper mentioned in the "Appendix A"

data Expr
  = Val Int
  | Add Expr Expr
  | Throw
  | Catch Expr Expr

eval :: Expr -> Maybe Int
eval (Val n) = Just n
eval (Add x y) = case eval x of
  Just n -> case eval y of
    Just m -> Just (n + m)
    Nothing -> Nothing
  Nothing -> Nothing
eval Throw = Nothing
eval (Catch x h) = case eval x of
  Just n -> Just n
  Nothing -> eval h

-- sample Expressions:
e1 :: Maybe Int
e1 = eval (Val 1) -- Just 1

e2 :: Maybe Int
e2 = eval (Val 2) -- Just 2

e3 :: Maybe Int
e3 = eval (Add (Val 1) (Val 2)) -- Just 3

data Code = HALT | PUSH Int Code | ADD Code | FAIL | MARK Code Code | UNMARK Code deriving (Show)

comp :: Expr -> Code
comp x = comp' x HALT

comp' :: Expr -> Code -> Code
comp' (Val n) c = PUSH n c
comp' (Add x y) c = comp' x (comp' y (ADD c))
comp' Throw c = FAIL
comp' (Catch x h) c = MARK (comp' h c) (comp' x (UNMARK c))

type Stack = [Elem]

data Elem = VAL Int | HAN Code deriving Show

t1 :: Code
t1 = comp (Val 1) -- PUSH 1 HALT

t2 :: Code
t2 = comp (Add (Val 1) (Val 2)) -- PUSH 1 (PUSH 2 (ADD HALT))

exec :: Code -> Stack -> Stack
exec HALT s = s
exec (PUSH n c) s = exec c (VAL n : s)
exec (ADD c) (VAL m : VAL n : s) = exec c (VAL (n + m) : s)
exec FAIL s = fail' s
exec (MARK c' c) s = exec c (HAN c' : s)
exec (UNMARK c) (VAL n : HAN _ : s) = exec c (VAL n : s)

fail' :: Stack -> Stack
fail' [ ] = [ ]

expr :: Expr
expr = (Add (Val 1) (Val 2))

code :: Code
code = comp expr   --PUSH 1 (PUSH 2 (ADD HALT))

-- empty stack
st :: Stack
st = []

result :: Stack
result = exec code st -- [VAL 3]