import Data.List.Split

sampleData = ['6','2','\n','2','1','\n']

myLines = splitOn "\n"

toInts :: String -> [Int]
toInts = map read . lines

main :: IO ()
main = do
    userInput <- getContents
    let numbers = toInts userInput
    print (sum numbers)
