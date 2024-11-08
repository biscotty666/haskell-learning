import Data.Char (isDigit)
import Data.List (transpose)
import System.IO

size :: Int
size = 3

type Grid = [[Player]]

data Player = O | B | X
              deriving (Eq, Ord, Show)

next :: Player -> Player
next O = X
next X = O
next B = B -- for completeness

empty :: Grid
empty = replicate size (replicate size B)

full :: Grid -> Bool
-- full = all (/= B) . concat -- original
-- full = all (all (/= B)) -- linter suggestion
full = all (notElem B) -- linter suggestion

-- O goes first
turn :: Grid -> Player
turn g = if os <= xs then O else X
            where
              os = length (filter (== O) ps)
              xs = length (filter (== X) ps)
              ps = concat g

wins :: Player -> Grid -> Bool
wins p g = any line (rows ++ cols ++ dias)
             where
               line = all (== p)
               rows = g
               cols = transpose g
               dias = [diag g, diag (map reverse g)]

-- Return main diagonal of a grid/matrix
diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [0..size-1]]

won :: Grid -> Bool
won g = wins O g || wins X g

{-
 Display the grid
-}
putGrid :: Grid -> IO ()
putGrid =
    putStrLn . unlines . concat . interleave bar . map showRow
    where bar = [replicate ((size*4)-1) '-']

-- ghci> putGrid [[B,O,O],[O,X,O],[X,X,X]]
--
--    |   |
--    | O | O
--    |   |
-- -----------
--    |   |
--  O | X | O
--    |   |
-- -----------
--    |   |
--  X | X | X
--    |   |

interleave :: a -> [a] -> [a]
interleave x [] = []
interleave x [y] = [y]
interleave x (y:ys) = y : x : interleave x ys

showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer
          where
            beside = foldr1 (zipWith (++))
            bar = replicate 3 "|"

showPlayer :: Player -> [String]
showPlayer O = ["   ", " O ","   "]
showPlayer B = ["   ", "   ","   "]
showPlayer X = ["   ", " X ","   "]

{-
 Making moves
-}
valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i < size^2 && concat g !! i == B

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

move :: Grid -> Int -> Player -> [Grid]
move g i p =
    -- if valid g i then [chop size (xs ++ [p] ++ ys)] else []
    [chop size (xs ++ [p] ++ ys) | valid g i]
    where (xs,B:ys) = splitAt i (concat g)

-- Get an index number from input for placement
getNat :: String -> IO Int
getNat prompt = do putStr prompt
                   xs <- getLine
                   if xs /= [] && all isDigit xs
                   then return (read xs)
                   else do putStrLn "ERROR: Invalid number"
                           getNat prompt

{-
 Human vs. Human
-}
tictactoe :: IO ()
tictactoe = run empty O

run :: Grid -> Player -> IO ()
run g p = do cls
             goto (1,1)
             putGrid g
             run' g p

cls :: IO ()
cls = putStr "\ESC[2J" -- control character to clear Screen

goto :: (Int, Int) -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

run' :: Grid -> Player -> IO ()
run' g p | wins O g = putStrLn "Player O wins!\n"
         | wins X g = putStrLn "Player X wins!\n"
         | full g = putStrLn "It's a draw!\n"
         | otherwise = do
               i <- getNat (prompt p)
               let i' = i - 1
               case move g i' p of
                   [] -> do putStrLn "ERROR: Invalid move"
                            run' g p
                   [g'] -> run g' (next p)

prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move: "

{-
 Computer vs Human
-}
data Tree a = Node a [Tree a] deriving Show

gameTree :: Grid -> Player -> Tree Grid
gameTree g p = Node g [gameTree g' (next p) | g' <- moves g p]

moves :: Grid -> Player -> [Grid]
moves g p | won g = []
          | full g = []
          | otherwise = concat [move g i p | i <- [0..((size^2)-1)]]

{-
 Pruning the Tree: Important to avoid memory overload

 This prune function produces a tree with a maximum depth of n

 The book estimates that a 3x3 grid tree should be okay, so the
 max is used in this program.
-}
prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]

depth :: Int
depth = 9

{-
 Minimax algorithm

 - Leaves (nodes w/o subtrees) are labelled with the winning
   player if there is one and only one blank
 - Other nodes )with subtrees) are labelled with the minimum
   or maximum of the player lables form the child node, depending
   on whose turn it is. On player O's turn, use minimum, for
   player X use maximum (player O played first)
-}

-- Label the gameTree
minimax :: Tree Grid -> Tree (Grid, Player)
minimax (Node g [])
    | wins O g = Node (g,O) []
    | wins X g = Node (g,X) []
    | otherwise = Node (g,B) []
minimax (Node g ts)
    | turn g == O = Node (g, minimum ps) ts'
    | turn g == X = Node (g, maximum ps) ts'
                    where
                        ts' = map minimax ts
                        ps = [p | Node (_,p) _ <- ts']

{-
 The best next move is to any grid with the same lagel as the root node.
 When there are more than one, select the first.
-}
bestmove :: Grid -> Player -> Grid
bestmove g p = head [g' | Node (g',p') _ <- ts, p' == best]
               where
                   tree = prune depth (gameTree g p)
                   Node (_,best) ts = minimax tree

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          play empty O

play :: Grid -> Player -> IO ()
play g p = do cls
              goto (1,1)
              putGrid g
              play' g p

play'' :: Grid -> Player -> IO ()
play'' g p
     | wins O g = putStrLn "Player O wins!\n"
     | wins X g = putStrLn "Player X wins!\n"
     | full g   = putStrLn "It's a draw!\n"
     | p == O   = do i <- getNat (prompt p)
                     case move g i p of
                      [] -> do putStrLn "ERROR: Invalid move"
                               play' g p
                      [g'] -> play g' (next p)
     | p == X   = do putStrLn "Plyer X is thinking..."
                     (play $! bestmove g p) (next p)
--

play' :: Grid -> Player -> IO ()
play' g p
   | wins O g = putStrLn "Player O wins!\n"
   | wins X g = putStrLn "Player X wins!\n"
   | full g   = putStrLn "It's a draw!\n"
   | p == O   = do i <- getNat (prompt p)
                   let i' = i - 1
                   case move g i' p of
                      []   -> do putStrLn "ERROR: Invalid move"
                                 play' g p
                      [g'] -> play g' (next p)
   | p == X   = do putStr "Player X is thinking... "
                   (play $! (bestmove g p)) (next p)
