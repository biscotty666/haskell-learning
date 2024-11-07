import Text.Read.Lex (numberToFixed)
import Data.Char (digitToInt, isDigit)
{-
The initial set-up:

    1: * * * * *
    2: * * * *
    3: * * *
    4: * *
    5: *

Take turns removing one or more stars from a single row.
The winner is the person who removes the last piece.

-}

{-
 Game Utilities
-}
-- Allow players to take turns
next :: Int -> Int
next 1 = 2
next 2 = 1

type Board = [Int]

initial :: Board
initial = [5,4,3,2,1]

finished :: Board -> Bool
finished = all (== 0)

valid :: Board -> Int -> Int -> Bool
valid board row num = board !! (row - 1) >= num
-- the subtraction is d/t zero-indexing

move :: Board -> Int -> Int -> Board
move board row num = [update r n | (r,n) <- zip [1..] board]
    where update r n = if r == row then n-num else n

{-
 IO Utilities
-}

-- Display a row, given a row number
putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (concat (replicate num "* "))

-- Display an entire board, assuming 5 lines
putBoard :: Board -> IO ()
putBoard [a,b,c,d,e] = do putRow 1 a
                          putRow 2 b
                          putRow 3 c
                          putRow 4 d
                          putRow 5 e

getDigit :: String -> IO Int
getDigit prompt = do putStr prompt
                     x <- getChar
                     newline
                     if isDigit x
                     then return (digitToInt x)
                     else do putStrLn "ERROR: Invalid Digit"
                             getDigit prompt

newline :: IO ()
newline = putChar '\n'

play :: Board -> Int -> IO ()
play board player = do
    newline
    putBoard board
    if finished board then
        do newline
           putStr "Player "
           putStr (show (next player))
           putStrLn " wins!"
    else
        do newline
           putStr "Player "
           -- putStrLn (show player)
           -- print was recommended by the linter
           print player
           row <- getDigit "Enter a row number: "
           num <- getDigit "Stars to remove: "
           if valid board row num
           then play (move board row num) (next player)
           else do newline
                   putStrLn "ERROR: Invalid move"
                   play board player

nim :: IO ()
nim = play initial 1
