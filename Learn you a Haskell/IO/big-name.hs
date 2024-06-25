import           Data.Char

main = do
    putStrLn "What's your first name?"
    firstName <- getLine
    putStrLn "What is your last name?"
    lastName <- getLine
    let bigFirstName = map toUpper firstName
        bigLastName = map toUpper lastName
    putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"
