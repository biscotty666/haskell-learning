positions :: Eq a => a -> [a] -> [Int]
positions n xs = [i | (x, i) <- zip xs [0..], x == n]

natural_positions :: Eq a => a -> [a] -> [Int]
natural_positions n xs = [i + 1 | (x, i) <- zip xs [0..], x == n]
