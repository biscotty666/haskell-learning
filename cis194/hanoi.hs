{-
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
-}
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n source target aux
  | n <= 0 = []
  | otherwise = hanoi (n - 1) source aux target ++
      ((source, target) : hanoi (n - 1) aux target source)
