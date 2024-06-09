doubleList :: [Integer] -> [Integer]
doubleList [] = []
doubleList (n:ns) = (2 * n) : doubleList ns

multiplyList :: Integer -> [Integer] -> [Integer]
multiplyList _ [] = []
multiplyList n (m:ms) = (n * m) : multiplyList n ms

takeInt :: Int -> [Int] -> [Int]
takeInt _ [] = []
takeInt 0 _ = []
takeInt n (m:ms) = m : takeInt (n - 1) ms

dropInt :: Int -> [Int] -> [Int]
dropInt 0 x = x
dropInt _ [] = []
dropInt n (x:xs) = dropInt (n - 1) xs

sumInt :: [Int] -> Int
sumInt [] = 0
sumInt (x:xs) = x + sumInt xs 

scanSum :: [Int] -> [Int]
scanSum [] = []
scanSum [x] = [x]
scanSum (x:y:xs) = x : scanSum ((x + y) : xs)

scanSum' :: [Int] -> [Int]
scanSum' [] = []
scanSum' [x] = [x]
scanSum' (x:xs) = x : scanSum'((x + sumInt ( takeInt 1 xs)) : dropInt 1 xs)
