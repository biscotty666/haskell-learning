import qualified Data.Map as Map
import Data.List  -- for `intercalate`

data Organ = Heart | Brain | Kidney | Spleen deriving (Show,Eq)

organs :: [Organ]
organs = [Heart,Heart,Brain,Spleen,Spleen,Kidney]

ids :: [Int]
ids = [2,7,13,14,21,24]

organPairs :: [(Int,Organ)]
organPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

-- ghci> Map.lookup 7 organCatalog
-- Just Heart

possibleDrawers :: [Int]
possibleDrawers = [1 .. 50]

getDrawerContents :: [Int] -> Map.Map Int Organ -> [Maybe Organ]
getDrawerContents ids catalog = map getContents ids
    where getContents = \id -> Map.lookup id catalog

availableOrgans :: [Maybe Organ]
availableOrgans = getDrawerContents possibleDrawers organCatalog

countOrgan :: Organ -> [Maybe Organ] -> Int
countOrgan organ available = length (filter
                                        (\x -> x == Just organ)
                                        available)

-- ghci> countOrgan Brain availableOrgans
-- 1
-- ghci> countOrgan Heart availableOrgans
-- 2

isSomething :: Maybe Organ -> Bool
isSomething Nothing = False
isSomething (Just _) = True

justTheOrgans :: [Maybe Organ]
justTheOrgans = filter isSomething availableOrgans

showOrgan :: Maybe Organ -> String
showOrgan Nothing = ""
showOrgan (Just organ) = show organ

organList :: [String]
organList = map showOrgan justTheOrgans

cleanList :: String
cleanList = intercalate ", " organList

-- ghci> organList
-- ["Heart","Heart","Brain","Spleen","Spleen","Kidney"]
-- ghci> cleanList
-- "Heart, Heart, Brain, Spleen, Spleen, Kidney"
--
-- Suppose you need to do several things to a value in a Maybe. The mad scientist has a more interesting project. You’ll be given a drawer ID. You need to retrieve an item from the drawer. Then you’ll put the organ in the appropriate container (a vat, a cooler, or a bag). Finally, you’ll put the container in the correct location. Here are the rules for containers and locations: For containers:
-- Brains go in a vat.
-- Hearts go in a cooler.
-- Spleens and kidneys go in a bag.
-- For locations:
--   Vats and coolers go to the lab.
--   Bags go to the kitchen.

data Container = Vat Organ | Cooler Organ | Bag Organ

instance Show Container where
    show (Vat organ) = show organ ++ " in a vat"
    show (Cooler organ) = show organ ++ " in a cooler"
    show (Bag organ) = show organ ++ " in a bag"

data Location = Lab | Kitchen | Bathroom deriving Show

organToContainer :: Organ -> Container
organToContainer Brain = Vat Brain
organToContainer Heart = Cooler Heart
organToContainer organ = Bag organ

placeInLocation :: Container -> (Location,Container)
placeInLocation (Vat a) = (Lab, Vat a)
placeInLocation (Cooler a) = (Lab, Cooler a)
placeInLocation (Bag a) = (Kitchen, Bag a)

process :: Organ -> (Location, Container)
process organ = placeInLocation (organToContainer organ)

report :: (Location,Container) -> String
report (location,container) = show container ++ " in the " ++ show location

processAndReport :: (Maybe Organ) -> String
processAndReport (Just organ) = report (process organ)
processAndReport Nothing = "error, id not found"

processRequest :: Int -> Map.Map Int Organ -> String
processRequest id catalog = processAndReport organ
    where organ = Map.lookup id catalog

-- ghci> processRequest 13 organCatalog
-- "Brain in a vat in the Lab"
-- ghci> processRequest 12 organCatalog
-- "error, id not found"
