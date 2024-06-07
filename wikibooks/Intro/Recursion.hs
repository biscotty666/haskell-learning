factorial 0 = 1
factorial n = n * factorial (n - 1)

factorial' n = go n 1
    where
    go n res
        | n > 1     = go (n - 1) (res * n)
        | otherwise = res

power x y = x ^ y

plusOne x = x + 1

addition x 0 = x
addition x y = plusOne (addition x (y - 1))
