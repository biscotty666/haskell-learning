import Data.List
import Data.String

type Grid = Matrix Value
type Matrix a = [Row a]
type Row a = [a]
type Value = Char

easy :: Grid
easy =  ["2....1.38",
         "........5",
         ".7...6...",
         ".......13",
         ".981..257",
         "31....8..",
         "9..8...2.",
         ".5..69784",
         "4..25...."]


gentle                :: Grid
gentle                =  [".1.42...5",
                          "..2.71.39",
                          ".......4.",
                          "2.71....6",
                          "....4....",
                          "6....74.3",
                          ".7.......",
                          "12.73.5..",
                          "3...82.7."]


diabolical            :: Grid
diabolical            =  [".9.7..86.",
                          ".31..5.2.",
                          "8.6......",
                          "..7.5...6",
                          "...3.7...",
                          "5...1.7..",
                          "......1.9",
                          ".2.6..35.",
                          ".54..8.7."]

unsolvable            :: Grid
unsolvable            =  ["1..9.7..3",
                          ".8.....7.",
                          "..9...6..",
                          "..72.94..",
                          "41.....95",
                          "..85.43..",
                          "..3...7..",
                          ".5.....4.",
                          "2..8.6..9"]

-- Minimal sized grid (17 values) with a unique solution:

minimal               :: Grid
minimal               =  [".98......",
                          "....7....",
                          "....15...",
                          "1........",
                          "...2....9",
                          "...9.6.82",
                          ".......3.",
                          "5.1......",
                          "...4...2."]

blank :: Grid
blank = replicate 9 (replicate 9 '.')

boxsize :: Int
boxsize = 3

values :: [Value]
values = ['1'..'9']

empty :: Value -> Bool
empty = (== '.')

single :: [a] -> Bool
single [_] = True
single _ = False

rows :: Matrix a -> [Row a]
-- rows m = m
rows = id

-- Note rows (rows m ) = m

cols :: Matrix a -> [Row a]
cols = transpose

-- transpose :: [[a]] -> [[a]]
-- transpose ([]:_) = []
-- transpose x = (map head x) : transpose (map tail x)

boxs :: Matrix a -> [Row a]
boxs = unpack . map cols . pack
       where
         pack = split . map split
         split = chop boxsize
         unpack = map concat . concat

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

lc = map (take 3) gentle
cc = map (take 3) (map (drop 3) gentle)
rc = map (drop 6) gentle
tr = map (take 3) (transpose gentle)
cr = map (take 3) (map (drop 3) (transpose gentle))
br = map (drop 6) (transpose gentle)

test list1 list2 = [ [n,m] | n <- list1,
                             m <- list2 ]

aBox list1 list2 = concat (concat [ [n,m] | n <- list1,
                                            m <- list2 ])

search :: [Int] -> [a] -> [a]
search = search' 0
    where search' _ [] _ = []
          search' i (j:js) xs = y : search' (j+1) js ys
            where (y:ys) = drop (j-i) xs

inBox :: Int -> Int -> Int
inBox n m 
        | ((n-1) `div` 3) == 0 && ((m-1) `div` 3) == 0 = 1
        | ((n-1) `div` 3) == 0 && ((m-1) `div` 3) == 1 = 2
        | ((n-1) `div` 3) == 0 && ((m-1) `div` 3) == 2 = 3
        | ((n-1) `div` 3) == 1 && ((m-1) `div` 3) == 0 = 4
        | ((n-1) `div` 3) == 1 && ((m-1) `div` 3) == 1 = 5
        | ((n-1) `div` 3) == 1 && ((m-1) `div` 3) == 2 = 6
        | ((n-1) `div` 3) == 2 && ((m-1) `div` 3) == 0 = 7
        | ((n-1) `div` 3) == 2 && ((m-1) `div` 3) == 1 = 8
        | ((n-1) `div` 3) == 2 && ((m-1) `div` 3) == 2 = 9
        | otherwise = error "Something wrong"
