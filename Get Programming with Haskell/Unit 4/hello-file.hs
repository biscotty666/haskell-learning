import System.IO

main:: IO ()
main = do
    helloFile <- openFile "hello.txt" ReadMode
    firstLine <- hGetLine helloFile
    putStrLn firstLine
    secondLine <- hGetLine helloFile
    goodbyFile <- openFile "goodbye.txt" WriteMode
    hPutStrLn goodbyFile secondLine
    hClose helloFile
    hClose goodbyFile
    putStrLn "done"
