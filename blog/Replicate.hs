replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x: replicate' (n - 1) x

replicate'' :: Int -> a -> [a]
replicate'' n x =
    if n <= 0
       then
          []
       else
          x : replicate'' (n - 1) x
