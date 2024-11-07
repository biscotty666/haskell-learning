{-# LANGUAGE LambdaCase #-}
import Control.Applicative
import Data.Char

newtype Parser a = P (String -> [(a,String)])

parse :: Parser a -> String -> [(a,String)]
parse (P p) = p

item :: Parser Char
item = P (\case
             [] -> []
             (x:xs) -> [(x,xs)])

-- ghci> parse item "abc"
-- [('a',"bc")]

instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap g p = P (\inp -> case parse p inp of
                    [] -> []
                    [(v,out)] -> [(g v, out)])
-- ghci> parse (fmap toUpper item) "abc"
-- [('A',"bc")]

instance Applicative Parser where
    -- pure :: a -> Parser a
    pure v = P (\inp -> [(v,inp)])

    -- <*> :: Parser (a -> b) -> Parser a -> Parser b
    pg <*> px = P (\inp -> case parse pg inp of
                [] -> []
                [(g,out)] -> parse (fmap g px) out)

-- ghci> parse (pure 1) "abc"
-- [(1,"abc")]
three :: Parser (Char,Char)
three = g <$> item <*> item <*> item -- suggested by the linter
            where g x y z = (x,z)
-- three = pure g <*> item <*> item <*> item
            -- where g x y z = (x,z)
-- ghci> parse three "abcdef"
-- [(('a','c'),"def")]
-- ghci> parse three "ab"
-- []
instance Monad Parser where
    p >>= f = P (\inp -> case parse p inp of
                [] -> []
                [(v,out)] -> parse (f v) out)

three' :: Parser (Char,Char)
three' = do x <- item
            item
            z <- item
            return (x,z)

{-
 Making choices - The Alternative class
-}
instance Alternative Parser where
    -- empty :: Parser a
    -- empty = P (\inp -> [])
    empty = P (const []) -- recommended by compiler

    -- (<|>) :: Parser a -> Parser a -> Parser a
    p <|> q = P (\inp -> case parse p inp of
                [] -> parse q inp
                [(v,out)] -> [(v,out)])

-- ghci> parse (empty <|> return 'd') "abc"
-- [('d',"abc")]
-- ghci> parse (item <|> return 'd') "abc"
-- [('a',"bc")]
