data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop
          | Or Prop Prop
          | XNOR Prop Prop

type Assoc k v = [(k, v)]

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k',v) <- t, k == k']
type Subst = Assoc Char Bool

eval :: Subst -> Prop -> Bool

eval _ (Const b)   = b
eval s (Var x)     = find x s
eval s (Not p)     = not (eval s p)
eval s (And p q)   = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q
eval s (Or p q)    = eval s p || eval s q
eval s (XNOR p q)  = eval s p == eval s q

vars :: Prop -> [Char]
vars (Const _)  = []
vars (Var x)    = [x]
vars (Not p)    = vars p
vars (And p q)  = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Or p q) = vars p ++ vars q
vars (XNOR p q) = vars p ++ vars q


bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss
          where bss = bools (n-1)

rmdups :: Eq a => [a] -> [a]
rmdups []     = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
           where vs = rmdups (vars p)

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]

-- example of use
p1 :: Prop
p1 = Or (Var 'A')  (Var 'A')
p2 :: Prop
p2 = Or (Var 'A') (Not (Var 'A'))
p3 :: Prop
p3 = XNOR (Var 'A') (Var 'A')
p4 :: Prop
p4 = XNOR (Var 'A') (Not(Var 'A'))
p5 :: Prop
p5 = XNOR (Not(Var 'A')) (Not(Var 'A'))
p6 :: Prop
p6 = Or (Not(Var 'A')) (Not(Var 'A'))
p7 :: Prop
p7 = XNOR (Imply(And (Var 'A') (Var 'B')) (Var 'C')) (Imply (Var 'A') (Imply (Var 'B') (Var 'C')))

{-
isTaut p1 - False
isTaut p2 - True
isTaut p3 - True
isTaut p4 - True
isTaut p5 - True
isTaut p6 - False
isTaut p7 - True
True
-}
