---
id: append-away
aliases: []
tags: []
---

import Prelude hiding (reverse)

reverse1 :: [a] -> [a]
reverse1 [] = []
reverse1 (x:xs) = reverse1 xs ++ [x]

reverse' :: [a] -> [a] -> [a]
-- reverse' [] ys = ys
-- reverse' (x:xs) ys = reverse' xs (x:ys)
-- The linter recommended the following
reverse' xs ys = foldl (flip (:)) ys xs

reverse :: [a] -> [a]
reverse xs = reverse' xs []

data Tree = Leaf Int | Node Tree Tree

flatten1 :: Tree -> [Int]
flatten1 (Leaf n) = [n]
flatten1 (Node l r) = flatten1 l ++ flatten1 r

flatten' :: Tree -> [Int] -> [Int]
flatten' (Leaf n) ns = n : ns
flatten' (Node l r) ns = flatten' l (flatten' r ns)

flatten :: Tree -> [Int]
flatten t = flatten' t []
