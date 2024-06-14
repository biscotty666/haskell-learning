aList = [1,3,5,7,8,9,24]
bList = [8,5,1,27,9,2,32]

multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

compareWithHundred :: (Num a, Ord a) => a -> Ordering
-- compareWithHundred x = compare 100 x
compareWithHundred = compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f y x = f x y


-- quicksort :: (Ord a) => [a] -> [a]
-- quicksort [] = []
-- quicksort (x:xs) = 
--     let smallerSorted = quicksort [a | a <- xs, a <= x]
--         biggerSorted = quicksort [a | a <- xs, a > x]
--     in smallerSorted ++ [x] ++ biggerSorted

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let smallerSorted = quicksort (filter (<=x) xs)
        biggerSorted = quicksort (filter (>x) xs)
    in smallerSorted ++ [x] ++ biggerSorted

-- largest number divisible by 3829
largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999..] )
    where p x = x `mod` 3829 == 0

-- sum of all odd squares smaller than 10,000
sumOddSquares :: (Integral a) => a
sumOddSquares = sum (takeWhile (< 10000) (filter odd (map (^2) [1..])))

{- Collatz sequences
 We take a natural number. If that number is even, we divide it by two. 
 If it's odd, we multiply it by 3 and then add 1 to that. 
 We take the resulting number and apply the same thing to it,
 which produces a new number and so on. In essence, we get a chain 
 of numbers. It is thought that for all starting numbers, the 
 chains finish at the number 1. So if we take the starting number 13, 
 we get this sequence: 13, 40, 20, 10, 5, 16, 8, 4, 2, 1. 13*3 + 1
 equals 40. 40 divided by 2 is 20, etc. We see that the chain has 10 terms.

 Now what we want to know is this: for all starting numbers between 1 and 100, 
 how many chains have a length greater than 15? First off, we'll 
 write a function that produces a chain:
-}

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n = n : chain (n `div` 2)
    | odd n = n : chain (n * 3 + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
    where isLong xs = length xs > 15

-- lambda
numLongChains' :: Int
numLongChains' = length (filter (\xs -> length xs > 15) (map chain [1..100]))

addThree :: (Num a) => a -> a -> a -> a
addThree x y z = x + y + z

 -- currying with lambdas
addThree' :: (Num a) => a -> a -> a -> a
addThree' = \x -> \y -> \z -> x + y + z

flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f = \x y -> f y x

-- folding
sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

-- The lambda function (\acc x -> acc + x) is the same as (+). 
-- We can omit the xs as the parameter because calling foldl (+) 0 
-- will return a function that takes a list.

sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys

-- the left fold's binary function has the accumulator as the first parameter 
-- and the current value as the second one (so \acc x -> ...), the right fold's 
-- binary function has the current value as the first parameter and the 
-- accumulator as the second one (so \x acc -> ...).

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

-- the foldl equivalent
map'' :: (a -> b) -> [a] -> [b]
map'' f xs = foldl (\acc x -> acc ++ [f x]) [] xs

maximum' :: (Ord a) => [a] -> a
maximum' = foldr1 (\x acc -> if x > acc then x else acc)

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

product' :: (Num a) => [a] -> a
product' = foldr1 (*)

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x : acc else acc) []

-- How many elements does it take for the sum of the roots of all natural
-- numbers to exceed 1000?
sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

{-
 dollar operator
 sqrt (3 + 4 + 9) is the same as sqrt $ 3 + 4 + 9 because $ has 
 the lowest precedence of any operator.
-}


