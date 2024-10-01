echo :: IO ()
echo = getLine >>= putStrLn

echoVerbose :: IO ()
echoVerbose = putStrLn "Enter a string to echo: " >>
              getLine >>= putStrLn

askForName :: IO ()
askForName = putStrLn "What is your name?"

nameStatement :: String -> String
nameStatement name = "Hello, " ++ name ++ "!"

-- chaining monad methods, >> is used when a return result
-- is not needed.
helloName :: IO ()
helloName = askForName >>
            getLine >>=
            (\name -> return (nameStatement name)) >>=
            putStrLn

helloNameDo :: IO ()
helloNameDo = do
    askForName
    name <- getLine
    putStrLn (nameStatement name)

main :: IO ()
main = echoVerbose
