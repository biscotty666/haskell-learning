type Pos = (Int, Int)

type Trans = Pos -> Pos

type Pair a = (a,a)

type Assoc k v = [(k,v)]

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k',v) <- t, k == k']

data Move = North | South | East | West deriving Show

move :: Move -> Pos -> Pos
move North (x,y) = (x, y+1)
move South (x,y) = (x, y-1)
move East (x,y) = (x+1, y)
move West (x,y) = (x-1, y)

--  move North (1,1)
--  (1,2)
moves :: [Move] -> Pos -> Pos
moves [] p = p
moves (m:ms) p = moves ms (move m p)

-- moves [North, North, East] (0,0)
-- (1,2)

rev :: Move -> Move
rev North = South
rev South = North
rev West = East
rev East = West

-- moves [North, North, East, rev East] (0,0)
-- (0,2)
