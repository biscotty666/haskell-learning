newtype Pair b a = Pair { getPair :: (a,b) }

instance Functor (Pair c) where
    fmap f (Pair (x,y)) = Pair (f x, y)

-- ghci> getPair $ fmap (*100) (Pair (2,3))
-- (200,3)
-- ghci> getPair $ fmap reverse (Pair ("london calling", 3))
-- ("gnillac nodnol",3)
