const' :: a -> (b -> a)
const' x = \_ -> x

not1 :: Bool -> Bool
not1 False = True
not1 True = False

not2 :: Bool -> Bool
not2 n | n == True = False
       | otherwise = True

not3 :: Bool -> Bool
not3 n = if n == True then False else True

not4 :: Bool -> Bool
not4 n = case n of
            True -> False
            False -> True
