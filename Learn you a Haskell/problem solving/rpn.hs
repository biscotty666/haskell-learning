import           Data.List

solveRPN :: String -> Float
solveRPN = head . foldl foldingFunction [] . words
    where   foldingFunction (x:y:ys) "*"    = (x * y):ys
            foldingFunction (x:y:ys) "+"    = (x + y):ys
            foldingFunction (x:y:ys) "-"    = (y - x):ys
            foldingFunction (x:y:ys) "/"    = (y / x):ys
            foldingFunction (x:y:ys) "^"    = (y ** x):ys
            foldingFunction (x:xs) "ln"     = log x:xs
            foldingFunction xs "sum"        = [sum xs]
            foldingFunction xs numberString = read numberString:xs

-- noticed that we added an extra class constraint of Read a to
-- the function declaration, because we call read on our string
-- to get the number. So this declaration means that the result
-- can be of any type that's part of the Num and Read typeclasses
-- (like Int, Float, etc.).

{-
ghci> solveRPN "2.7 ln"
0.9932518
ghci> solveRPN "10 10 10 10 sum 4 /"
10.0
ghci> solveRPN "10 10 10 10 10 sum 4 /"
12.5
ghci> solveRPN "10 2 ^"
100.0
-}
