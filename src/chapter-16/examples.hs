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
leaves = countLeaves t  -- 4

nodes :: Int
nodes = countNodes t    -- 3
