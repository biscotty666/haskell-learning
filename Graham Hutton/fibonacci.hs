fibs :: Int -> Int
fibs 1 = 1
fibs 2 = 2
fibs n = fibs (n-1) + fibs (n-2)

fibList' :: Int -> [Int]
fibList' 0 = []
fibList' n = fibs n : fibList' (n-1)

fibList :: Int -> [Int]
fibList  = reverse . fibList'
