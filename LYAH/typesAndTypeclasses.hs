factorial :: Integer -> Integer
factorial n = product [1..n]

circumference :: Float -> Float
circumference r = 2 * pi * r

circumference' :: Double -> Double
circumference' r = 2 * pi * r

data Answer = Yes | No | Unknown
    deriving Show

answers :: [Answer]
answers = [Yes, No, Unknown]

flip' :: Answer -> Answer
flip' Yes     = No
flip' No      = Yes
flip' Unknown = Unknown

data Shape = Circle Float
           | Rect Float Float

square :: Float -> Shape
square n = Rect n n

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect x y) = x * y

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv m n = Just (m `div` n)

safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead xs = Just (head xs)

-- Recursive Types
-- Nat is a new data type with constructors Zero :: Nat and Succ :: Nat -> Nat
data Nat = Zero | Succ Nat
  deriving Show

nat2int :: Nat -> Int
nat2int Zero    = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

add :: Nat -> Nat -> Nat
-- add m n = int2nat (nat2int m + nat2int n)
add Zero n = n
add (Succ m) n = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult Zero m = Zero
mult (Succ n) m = add (mult n m) m
