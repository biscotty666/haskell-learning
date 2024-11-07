init' :: [a] -> [a]
init' [] = []
init' xs = reverse (drop 1 (reverse xs))

bottom' :: Bool -> Bool
bottom' = undefined

f44 :: () -> Integer
f44 () = 44

unit :: a -> ()
unit _ = ()
