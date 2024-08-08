data CMaybe a = CNothing | CJust Int a deriving (Show)

instance Functor CMaybe where
    fmap f CNothing          = CNothing
    fmap f (CJust counter x) = CJust (counter+1) (f x)

myAction :: IO String
myAction = do
    a <- getLine
    b <- getLine
    return $ a ++ b

myAction' :: IO String
myAction' = (++) <$> getLine <*> getLine
