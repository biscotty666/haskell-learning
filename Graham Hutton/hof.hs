twice :: (a -> a) -> a -> a
twice f x = f (f x)

mapLC :: (a -> a) -> [a] -> [a]
mapLC f xs = [f x | x <- xs]

mapR :: (a -> a) -> [a] -> [a]
mapR f [] = []
mapR f (x:xs) = f x : map f xs

filterLC :: (a -> Bool) -> [a] -> [a]
filterLC p xs = [x | x <- xs, p x]

filterR :: (a -> Bool) -> [a] -> [a]
filterR p [] = []
filterR p (x:xs)
    | p x       = x : filterR p xs
    | otherwise = filterR p xs

reverseR :: [Int] -> [Int]
reverseR [] = []
reverseR (x:xs) = reverseR xs ++ [x]

