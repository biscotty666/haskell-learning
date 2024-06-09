qsort :: Ord a => [a] -> [a]
qsort []    = []
qsort (x:xs) =
    qsort smaller ++ [x] ++ qsort larger
    where
        smaller = [a | a <- xs, a <= x]
        larger = [b | b <- xs, b > x]

replic :: Int -> a -> [a]
replic 0    _   = []
replic n a = a : replic (n - 1) a

index :: [a] -> Int -> a
index (x:_) 0 = x 
index (_:xs) n = index xs (n-1)

{-
 - Helper function for insertion sort
 - Insert an integer into a list. Sorted list as input.
-}
insert :: Int -> [Int] -> [Int] -- sorted list as input
insert n []     = [n]
insert n (x:xs) = 
    if n <= x
    then
        n : x : xs
    else
        x : insert n xs

{-
 - Insertion sort
-}

isort :: [Int] -> [Int]
isort []    = []
isort (x:xs) = insert x (isort xs)


 -- Helper function for merge sort
 -- Merge two sorted lists
merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) = 
    if x <= y
    then
        x : merge xs (y:ys)
    else
        y : merge (x:xs) ys

halve :: [Int] -> ([Int],[Int])
halve [] = ([],[])
halve xs = ((take (length xs `div` 2) xs), (drop (length xs `div` 2) xs))

-- Merge sort
mSort :: [Int] -> [Int]
mSort [] = []
mSort [x] = [x]
mSort xs = merge (mSort ys) (mSort zs)
               where (ys, zs) = halve xs
