testNum :: Integer
testNum = 12345

getLast :: Integer -> Integer
getLast n
    | n <= 0 = 0
    | otherwise = n `mod` 10

dropLast :: Integer -> Integer
dropLast n
    | n <= 0 = 0
    | otherwise = n `div` 10


toDigits :: Integer -> [Integer]
toDigits n
    | n <= 0 = []
    | otherwise = toDigits (dropLast n) ++ [(getLast n)]

doubleEveryOtherFwd :: [Integer] -> [Integer]
doubleEveryOtherFwd [] = []
doubleEveryOtherFwd [x] = [x]
doubleEveryOtherFwd (x:y:rest) = x : (y * 2) : doubleEveryOtherFwd rest

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOtherFwd . reverse

flatList :: [Integer] -> [Integer]
flatList [] = []
flatList (x:xs) = (dropLast x) : (getLast x) : flatList xs

sumDigits :: [Integer] -> Integer
sumDigits xs = foldl (+) 0 (flatList xs)

isValid :: Integer -> Bool
isValid n = n `mod` 10 == 0

validate :: Integer -> Bool
validate = isValid . sumDigits . doubleEveryOther . toDigits
