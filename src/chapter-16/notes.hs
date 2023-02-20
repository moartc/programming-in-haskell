data Tree = Leaf Int | Node Tree Tree

countLeaves :: Tree -> Int
countLeaves (Leaf _) = 1
countLeaves (Node l r) = countLeaves l + countLeaves r

countNodes :: Tree -> Int
countNodes (Leaf _) = 0
countNodes (Node l r) = 1 + countNodes l + countNodes r

t :: Tree
t = Node (Node (Leaf 1) (Leaf 2)) (Node (Leaf 3) (Leaf 4))

leaves :: Int
leaves = countLeaves t -- 4

nodes :: Int
nodes = countNodes t -- 3

-- Exercise 11

data Expr = Val Int | Add Expr Expr deriving (Show)

eval :: Expr -> Int
eval (Val n) = n
eval (Add x y) = eval x + eval y

type Stack = [Int]

type Code = [Op]

data Op = PUSH Int | ADD deriving (Show)

exec :: Code -> Stack -> Stack
exec [] s = s
exec (PUSH n : c) s = exec c (n : s)
exec (ADD : c) (m : n : s) = exec c (n + m : s)

comp :: Expr -> Code
comp (Val n) = [PUSH n]
comp (Add x y) = comp x ++ comp y ++ [ADD]

e :: Expr
e = Add (Add (Val 2) (Val 3)) (Val 4)   -- Add (Add (Val 2) (Val 3)) (Val 4)

t1 :: Int
t1 = eval e -- g

t2 :: Code
t2 = comp e -- [PUSH 2,PUSH 3,ADD,PUSH 4,ADD]


comp' :: Expr -> [Op] -> [Op]
comp' e c = comp e ++ c

t3 = comp' e [PUSH  1]  -- [PUSH 2,PUSH 3,ADD,PUSH 4,ADD,PUSH 1]

comp'' :: Expr -> [Op] -> [Op]
comp'' (Val x) c = PUSH x : c
comp'' (Add x y) c = comp'' x (comp'' y (ADD : c))

t4 = comp'' e [PUSH 1]  -- [PUSH 2,PUSH 3,ADD,PUSH 4,ADD,PUSH 1]

