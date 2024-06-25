import           System.IO

main = do
    contents <- readFile "girlfriend.txt"
    putStr contents


-- withFile "girlfriend.txt" ReadMode (\handle -> do
--   contents <- hGetContents handle
--   putStr contents)

--     handle <- openFile "girlfriend.txt" ReadMode
--     contents <- hGetContents handle
--     putStr contents
--     hClose handle
