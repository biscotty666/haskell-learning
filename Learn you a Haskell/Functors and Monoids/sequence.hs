sequenceA' :: (Applicative f) => [f a] -> f [a]
sequenceA' [] = pure []
sequenceA' (x:xs) = (:) <$> x <*> sequenceA xs

sequenceA'' :: (Applicative f) => [f a] -> f [a]
sequenceA'' = foldr (liftA2 (:)) (pure [])


-- ghci> map (\f -> f 7) [(>4),(<10),odd]
-- [True,True,True]
-- ghci> and $ map (\f -> f 7) [(>4),(<10),odd]
-- True
--
-- ghci> sequenceA [(>4),(<10),odd] 7
-- [True,True,True]
-- ghci> and $ sequenceA [(>4),(<10),odd] 7
-- True
