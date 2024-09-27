data Tree a = Leaf a | Node (Tree a) a (Tree a)

--          5
--       3     7
--      1 4   6 9

-- Sample tree
t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

-- Test for existence of value
occurs :: Eq a => a -> Tree a -> Bool
occurs x (Leaf y)       = x == y
occurs x (Node l y r)   = x == y || occurs x l || occurs x r

flatten :: Tree a -> [a]
flatten (Leaf x)     = [x]
flatten (Node l x r) = flatten l ++ [x] ++ flatten r

-- make occurs more efficient by  not traversing both sides
-- this assumes the tree is a search tree so that, when flattened
-- the values are ordered

occurs' :: Ord a => a -> Tree a -> Bool
occurs' x (Leaf y)                  = x == y
occurs' x (Node l y r)  | x == y    = True
                        | x < y     = occurs' x l
                        | otherwise = occurs' x r
