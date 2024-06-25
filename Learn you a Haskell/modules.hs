import           Data.List
import qualified Data.Map       as Map
import           Geometry.GeoEx

aList = [1,3,5,7,8,9,24]
bList = [8,5,1,27,9,2,32]
cList = [1,2,1,3,1,6,3,9]
aString = "Jello World!"

-- nub for unique elements in a list
numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

-- search a list for a sublist
search' :: (Eq a) => [a] -> [a] -> Bool
search' needle haystack =
    let nlen = length needle
    in foldl (\acc x -> if take nlen x == needle then True else acc) False (tails haystack)

phoneBook =
    [("betty","555-2938")
    ,("bonnie","452-2928")
    ,("patsy","493-2928")
    ,("lucille","205-2928")
    ,("wendy","939-8282")
    ,("penny","853-2492")
    ]

{- error from empty list if not found
findKey :: (Eq k) => k -> [(k,v)] -> v
findKey key xs = snd . head . filter (\(k,v) -> key == k) $ xs
-}

findKeyRec :: (Eq k) => k -> [(k,v)] -> Maybe v
findKeyRec key [] = Nothing
findKeyRec key ((k,v) : xs) = if key == k
                              then Just v
                              else findKeyRec key xs

findKeyFold :: (Eq k) => k -> [(k,v)] -> Maybe v
findKeyFold key = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing
