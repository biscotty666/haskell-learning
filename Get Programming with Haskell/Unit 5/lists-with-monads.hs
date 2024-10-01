import Control.Monad -- used for guard

powersOfTwo :: Int -> [Int]
powersOfTwo n = do
    value <- [1 .. n]
    return (2^value)

-- The same with map and lambda
powersOfTwoMap :: Int -> [Int]
powersOfTwoMap n = map (\x -> 2^x) [1 .. n]

powersOfTwoLC :: Int -> [Int]
powersOfTwoLC n = [x^2 | x <- [1..n]]

powersOfTwoAndThree :: Int -> [(Int,Int)]
powersOfTwoAndThree n = do
    value <- [1 .. n]
    let powersOfTwo = 2^value
    let powersOfThree = 3^value
    return (powersOfTwo, powersOfThree)

powersOfTwoAndThree' :: Int -> [(Int,Int)]
powersOfTwoAndThree' n = [(x^2,x^3) | x <- [1 .. n]]

-- ghci> powersOfTwoAndThree 5
-- [(2,3),(4,9),(8,27),(16,81),(32,243)]

-- the above operated on a single list.
-- if operating on two lists, all possible
-- combinations are printed

allEvenOdds :: Int -> [(Int,Int)]
allEvenOdds n = do
    evenValue <- [2,4 .. n]
    oddValue <- [1,3 .. n]
    return (evenValue,oddValue)

-- ghci> allEvenOdds 5
-- [(2,1),(2,3),(2,5),(4,1),(4,3),(4,5)]

evensGuard :: Int -> [Int]
evensGuard n = do
    value <- [1 .. n]
    guard(even value)
    return value

allEvenOdds' :: Int -> [(Int,Int)]
allEvenOdds' n = [(evens,odds) | evens <- [2,4..n], odds <- [1,3..n]]

evensGuard' :: Int -> [Int]
evensGuard' n = [val | val <- [1..n], even val]
