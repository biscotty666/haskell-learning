{-
First declare type for arithmetic operators
-}
data Op = Add | Sub | Mul | Div

instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

{-
Declare type for numeric expressions, can either
be an integer value or the application of an operator
to two expressions
-}
data Expr = Val Int | App Op Expr Expr

instance Show Expr where
    show (Val n)    = show n
    show (App o l r) = brak l ++ show o ++ brak r
                       where
                         brak (Val n) = show n
                         brak e       = "(" ++ show e ++ ")"

{-
ghci> show (App Add (Val 1) (App Mul (Val 2) (Val 3)))
"1+(2*3)"
-}
{-
Evaluate if possible
-}
values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

eval :: Expr -> [Int]
eval (Val n)     = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l,
                                  y <- eval r,
                                  valid o x y]

{-
ghci> eval (App Add (Val 2) (Val 3))
[5]
ghci> eval (App Sub (Val 2) (Val 3))
[]
-}

{-
Combinatorial functions
-}

-- return all subsequences of a list
subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x:) yss -- embed into nested list
              where yss = subs xs

-- all possible ways of inserting a new element into a list
interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

-- all permutations of a list
perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

{-
ghci> subs [1,2,3]
[[],[3],[2],[2,3],[1],[1,3],[1,2],[1,2,3]]
ghci> interleave 1 [2,3,4]
[[1,2,3,4],[2,1,3,4],[2,3,1,4],[2,3,4,1]]
ghci> perms [1,2,3]
[[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]
-}

-- combine them to list all possible ways of selecting
-- zero or or elements in any order
choices :: [a] -> [[a]]
choices = concat . map perms . subs

{-
ghci> choices [1,2,3]
[[],[3],[2],[2,3],[3,2],[1],[1,3],[3,1],[1,2],[2,1],[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]
-}

mye :: Expr
mye = App Mul (App Add (Val 1) (Val 50)) (App Sub (Val 25) (Val 10))

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]

{-
ghci> show mye
"(1+50)*(25-10)"
ghci> solution mye [1,3,7,10,25,50] 765
True
-}
