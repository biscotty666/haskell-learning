import Data.List

testPermutations = permutations "abc"

revWords :: String -> String
revWords input = (unwords . reverse . words) input

helloWorld = putStrLn "Hello World"
