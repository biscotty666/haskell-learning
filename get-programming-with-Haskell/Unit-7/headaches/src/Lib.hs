module Lib where

myTake :: Int -> [a] -> [a]
myTake 0 _ = []
myTake _ [] = []
myTake n xs = head xs : myTake (n-1) (tail xs)

-- ghci> myTake 2 [1,2,3] :: [Int]
-- [1,2]
-- ghci> myTake 4 [1,2,3] :: [Int]
-- [1,2,3,*** Exception: Prelude.head: empty list
-- CallStack (from HasCallStack):
--   error, called at libraries/base/GHC/List.hs:1644:3 in base:GHC.List
--   errorEmptyList, called at libraries/base/GHC/List.hs:87:11 in base:GHC.List
--   badHead, called at libraries/base/GHC/List.hs:83:28 in base:GHC.List
--   head, called at /home/biscotty/Learning/Haskell/get-programming-with-Haskell/Unit-7/headaches/src/Lib.hs:5:15 in main:Lib

myTakePM :: Int -> [a] -> [a]
myTakePM 0 _ = []
myTakePM _ [] = []
myTakePM n (x:xs) = x : myTakePM (n-1) xs

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:_) = Just x

-- ghci> maybeHead [1,2,3 :: Int]
-- Just 1
-- ghci> maybeHead []
-- ghci> (+2) <$> maybeHead [1 :: Int]
-- Just 3
-- ghci> (+2) <$> maybeHead []
-- Nothing
-- ghci> (:) <$> maybeHead [1,2,3 :: Int] <*> Just []
-- Just [1]
-- ghci> (:) <$> maybeHead [] <*> Just []
-- Nothing

myTakeSafer :: Int -> Maybe [a] -> Maybe [a]
myTakeSafer 0 _ = Just []
myTakeSafer _ (Just []) = Just []
myTakeSafer _ Nothing = Nothing
myTakeSafer n (Just xs) = (:) <$> maybeHead xs
                              <*> myTakeSafer (n-1) (Just (tail xs))

-- ghci> myTakeSafer 3 (Just [1,2,3 :: Int])
-- Just [1,2,3]
-- ghci> myTakeSafer 6 (Just [1,2,3 :: Int])
-- Nothing

eitherHead :: [a] -> Either String a
eitherHead [] = Left "There is no head because the list is empty."
eitherHead (x:_) = Right x

-- ghci> eitherHead intExample
-- Right 1
-- ghci> eitherHead intExampleEmpty
-- Left "There is no head because the list is empty."
-- ghci> eitherHead charExample
-- Right 'c'
-- ghci> eitherHead charExampleEmpty
-- Left "There is no head because the list is empty."
-- ghci> (+ 1) <$> (eitherHead intExample)
-- Right 2
-- ghci> (+ 1) <$> (eitherHead intExampleEmpty)
-- Left "There is no head because the list is empty."

-- isPrime :: Int -> Either String Bool
-- isPrime n
--     | n < 2 = Left "Numbers less than 2 are not primes"
--     | n > maxN = Left "Value exceeds limits of checker"
--     | otherwise = Right (n `elem` primes)

-- ghci> isPrime 5
-- Right True
-- ghci> isPrime 6
-- Right False
-- ghci> isPrime 100
-- Left "Value exceeds limits of checker"
-- ghci> isPrime (-29)
-- Left "Numbers less than 2 are not primes"

data PrimeError = TooLarge | InvalidValue

instance Show PrimeError where
    show TooLarge = "Value exceed max bound"
    show InvalidValue = "Value is not valid candidate for prime checking"

isPrime :: Int -> Either PrimeError Bool
isPrime n
    | n < 2 = Left InvalidValue
    | n > maxN = Left TooLarge
    | otherwise = Right (n `elem` primes)

-- ghci> isPrime 99
-- Left Value exceed max bound
-- ghci> isPrime 0
-- Left Value is not valid candidate for prime checking
-- ghci> isPrime 7
-- Right True

displayResult :: Either PrimeError Bool -> String
displayResult (Right True) = "It's prime"
displayResult (Right False) = "It's composite"
displayResult (Left primeError) = show primeError

primes :: [Int]
primes = [2,3,5,7]

maxN :: Int
maxN = 10

intExample :: [Int]
intExample = [1,2,3]

intExampleEmpty :: [Int]
intExampleEmpty = []

charExample :: [Char]
charExample = "cat"

charExampleEmpty :: [Char]
charExampleEmpty = ""
