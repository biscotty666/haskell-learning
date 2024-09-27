-- Natural number definition

data Nat = Zero | Succ Nat deriving Show

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

-- instead of
-- add :: Nat -> Nat -> Nat
-- add m n = int2nat (nat2int m + nat2int n)
-- it is more efficient to skip conversion by
-- defining recursively

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)
