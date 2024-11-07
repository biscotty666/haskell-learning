import GHC.Float (expts10)
import System.Environment (executablePath)
{-
 Game of Life

 The board is 5x5 and wraps left-right and up-down
 as a torus

 Each square may or may not be inhabited by a living cell.

 The board iterates according to these rules:

 1. A living cell survives if it has exactly 2 or 3 neighbor living cells
 2. An empty square generates a living cell if it has exactly 3 neighbor
     living cells
-}

-- Screen Utilities

cls :: IO ()
cls = putStr "\ESC[2J" -- control character to clear Screen

-- positions (x,y) numbered from top left
type Pos = (Int,Int)

-- places a string at a given position on the screen
writeat :: Pos -> String -> IO ()
writeat p xs = do goto p
                  putStr xs

goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

{-
 Allow for variation in the size of the board
-}

width :: Int
width = 10

height :: Int
height = 10

type Board = [Pos]

{-
 An example board which will continually shift diagonally

    ===========
    | | | | | |
    | | | |o| |
    | |o| |o| |
    | | |o|o| |
    | | | | | |
    ===========

-}
glider :: Board
glider = [(4,2),(2,3),(4,3),(3,4),(4,4)]

newline :: IO ()
newline = putChar '\n'

showcells :: Board -> IO ()
showcells b = sequence_ ([writeat p "0" | p <- b] ++ [newline])
-- writeat is an IO action which does not return
-- a result. sequence_ is used to execute
-- the list of actions.

isAlive :: Board -> Pos -> Bool
isAlive b p = elem p b

isEmpty :: Board -> Pos -> Bool
isEmpty b p = not (isAlive b p)

wrap :: Pos -> Pos
wrap (x,y) = (((x-1) `mod` width) + 1,
              ((y-1) `mod` height) + 1)

wrap' :: Pos -> Pos
wrap' (x,y) = (x `mod` width,
               y `mod` height)

neighbors :: Pos -> [Pos]
neighbors (x,y) = map wrap [(x-1,y-1),(x,y-1),(x+1,y-1),
                            (x-1,y),(x+1,y),
                            (x-1,y+1),(x,y+1),(x+1,y+1)]

liveNeighbors :: Board -> Pos -> Int
liveNeighbors b = length . filter (isAlive b) . neighbors

survivors :: Board -> [Pos]
survivors b = [p | p <- b, elem (liveNeighbors b p) [2,3]]

{-
births :: Board -> [Pos]
births b = [(x,y) | x <- [1..width],
                    y <- [1..height],
                    isEmpty b (x,y),
                    liveNeighbors b (x,y) == 3]
-}
births :: Board -> [Pos]
-- births b = [p | p <- rmDups (concat (map neighbors b)), from book
births b = [p | p <- rmDups (concatMap neighbors b),
                isEmpty b p,
                liveNeighbors b p == 3]

rmDups :: Eq a => [a] -> [a]
rmDups [] = []
rmDups (x:xs) = x : rmDups (filter (/= x) xs)

nextGen :: Board -> Board
nextGen b = survivors b ++ births b

life :: Board -> IO ()
life b = do cls
            showcells b
            wait 500000
            life (nextGen b)

wait :: Int -> IO ()
wait n = sequence_ [return () | _ <- [1..n]]
