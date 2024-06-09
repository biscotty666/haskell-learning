factorial 0 = 1
factorial n = n * factorial (n - 1)

factorial' n = go n 1
    where
    go n res
        | n > 1     = go (n - 1) (res * n)
        | otherwise = res

doubleFactorial 0 = 1
doubleFactorial 1 = 1
doubleFactorial n = n * doubleFactorial (n - 2) 

mult _ 0 = 0
mult n m = (mult n (m - 1)) + n  -- recurse: multiply by one less and add a copy

power x y = x ^ y

plusOne x = x + 1

addition x 0 = x
addition x y = plusOne (addition x (y - 1))

length' :: [a] -> Int
length' []       = 0
length' (x:xs)   = 1 + length xs

accumLength :: [a] -> Int
accumLength xs = go 0 xs
    where
    go acc []   = acc
    go acc (_:xs) = go (acc + 1) xs

replic :: Int -> a -> [a]
replic 0 _ = [] 
replic n s = s : replic (n - 1) s 

ind :: [a] -> Int -> a
ind [] _ = error "Index too large"
ind (x:_) 0 = x
ind (x:xs) n  = ind xs (n - 1)

zip' :: [a] -> [b] -> [(a, b)]
zip' []  _   = []
zip' _   []  = []
zip' (x:xs) (y:ys) = (x, y) : zip xs ys

