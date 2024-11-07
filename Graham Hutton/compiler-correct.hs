import Data.Bits (Bits(xor))
-- Review
data Expr = Val Int | Add Expr Expr

eval :: Expr -> Int
eval (Val n) = n
eval (Add x y) = eval x + eval y

type Stack = [Int]

type Code = [Op]

data Op = PUSH Int | ADD deriving Show

exec :: Code -> Stack -> Stack
exec []           s       = s
exec (PUSH n : c) s       = exec c (n : s)
exec (ADD : c)    (m:n:s) = exec c (n+m : s)
