data User = User
    { name :: String
    , gamerId :: Int
    , score :: Int
    } deriving Show

serverUsername :: Maybe String
serverUsername = Just "Sue"

serverGamerId :: Maybe Int
serverGamerId =  Just 1337

serverScore :: Maybe Int
serverScore = Just 9001

-- ghci> User <$> serverUsername <*> serverGamerId <*> serverScore
-- Just (User {name = "Sue", gamerId = 1337, score = 9001})

readInt :: IO Int
readInt = read <$> getLine

main :: IO ()
main = do
    putStrLn "Enter a username, gamerId and score"
    user <- User <$> getLine <*> readInt <*> readInt
    print user

-- ghci> main
-- Enter a username, gamerId and score
-- Beth
-- 513
-- 5116
-- User {name = "Beth", gamerId = 513, score = 5116}

-- Generate test data

testNames :: [String]
testNames = ["John Smith"
            ,"Robert'); DROP TABLE Students;--"
            ,"Christina NULL"
            ,"Randall Munroe"]

testIds :: [Int]
testIds = [1337
          ,0123
          ,999999]

testScores :: [Int]
testScores = [0
             ,100000
             ,-99999]

testData :: [User]
testData = pure User <*> testNames
                    <*> testIds
                    <*> testScores

-- ghci> length testData
-- 36
-- ghci> take 3 testData
-- [User {name = "John Smith", gamerId = 1337, score = 0},User {name = "John Smith", gamerId = 1337, score = 100000},User {name = "John Smith", gamerId = 1337, score = -99999}]
