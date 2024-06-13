aList = [1,3,5,7,8,9,24]
bList = [8,5,1,27,9,2,32]

-- Recursion with pattern matching and guards

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Empty List"
maximum' [x] = x
maximum' (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximum' xs

maxi :: (Ord a) => [a] -> a
maxi [] = error "Empty List"
maxi [x] = x
maxi (x:xs) = max x (maxi xs)

{- Note: Num is not a subclass of Ord. That means that what constitutes 
   for a number doesn't really have to adhere to an ordering. So that's 
   why we have to specify both the Num and Ord class constraints when doing 
   addition or subtraction and also comparison.
-}

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0    = []
    | otherwise = x:replicate' (n - 1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0  = []
take' _ []    = []
take' n (x:xs) = x : take' (n - 1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs)
    | a == x = True
    | otherwise = a `elem'` xs

-- Quicksort

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in smallerSorted ++ [x] ++ biggerSorted
