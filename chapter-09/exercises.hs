data Op = Add | Sub | Mul | Div
instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = True
valid Mul _ _ = True
valid Div x y = y /= 0 && x `mod` y == 0

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

data Expr = Val Int | App Op Expr Expr
instance Show Expr where
    show (Val n)     = show n
    show (App o l r) = brak l ++ show o ++ brak r
                       where
                         brak (Val n) = show n
                         brak e       = "(" ++ show e ++ ")"

subs :: [a] -> [[a]]
subs []     = [[]]
subs (x:xs) = yss ++ map (x:) yss
              where yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x []     = [[x]]
interleave x (y:ys) = (x : y : ys) : map (y:) (interleave x ys)

perms :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

choices :: [a] -> [[a]]
choices = concat . map perms . subs
-- > choices [1,2,3]
-- < [[],[3],[2],[2,3],[3,2],[1],[1,3],[3,1],[1,2],[2,1],
--   [1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]

-- Exercises 1
choices' :: [a] -> [[a]]
choices' xs = [c | s <- subs xs, c <- perms s]

-- Exercise 2
removeFirst :: Eq a => a -> [a] -> [a]
removeFirst _ [] = []
removeFirst v (x:xs)  = if v == x
                        then xs
                        else x : removeFirst v xs


-- solution is different from the one given in appendix, but it seems to work.
isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] ys     = True
isChoice xs (y:ys) = isChoice (removeFirst y xs) ys
isChoice _ _       = False
{-
Expected outputs
isChoice [1] [1] - True
isChoice [] [1]  - True
isChoice [1] [0] - False
isChoice [1] []  - False
isChoice [1,2] [1] - False
isChoice [1] [1,2] - True
isChoice [3,2,1] [5,1,2,3] - True
-}

-- Exercise 4

values :: Expr -> [Int]
values (Val n)     = [n]
values (App _ l r) = values l ++ values r

eval :: Expr -> [Int]
eval (Val n)     = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l,
                                  y <- eval r,
                                  valid o x y]

type Result = (Expr, Int)

results :: [Int] -> [Result]
results []  = []
results [n] = [(Val n,n) | n > 0]
results ns  = [res | (ls,rs) <- split ns,
                      lx     <- results ls,
                      ry     <- results rs,
                      res    <- combine' lx ry]

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [e | ns' <- choices ns, (e,m) <- results ns', m == n]

combine' :: Result -> Result -> [Result]
combine' (l,x) (r,y) = [(App o l r, apply o x y) | o <- ops, valid' o x y]

-- Brute force solution
split :: [a] -> [([a],[a])]
split []      = []
split [_]     = []
split (x: xs) = ([x], xs) : [(x:ls,rs) | (ls,rs) <- split xs]
-- > split [1,2,3,4]
-- < [([1],[2,3,4]),([1,2],[3,4]),([1,2,3],[4])]

exprs :: [Int] -> [Expr]
exprs []  = []
exprs [n] = [Val n]
exprs ns  = [e | (ls,rs) <- split ns,
                l <- exprs ls,
                r <- exprs rs,
                e <- combine l r]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

ops :: [Op]
ops = [Add, Sub, Mul, Div]

valid' :: Op -> Int -> Int -> Bool
valid' Add x y = x <= y
valid' Sub x y = x > y
valid' Mul x y = x /= 1 && y /= 1 && x <= y
valid' Div x y = y /= 1 && x `mod` y == 0


allsols ns = [e | ns' <- choices ns, e <- exprs ns']
allvalid ns = [x | x <- allsols ns, eval x /= []]
