data Box a = Box a deriving Show

wrap :: a -> Box a
wrap x = Box x

unwrap :: Box a -> a
unwrap (Box x) = x

data Triple a = Triple a a a deriving Show

type Point3D = Triple Double

aPoint :: Point3D
aPoint = Triple 0.1 53.2 12.3

type FullName = Triple String

aPerson :: FullName
aPerson = Triple "Howard" "Philips" "Lovecraft"

type Initials = Triple Char

initials :: Initials
initials = Triple 'H' 'P' 'L'

first :: Triple a -> a
first (Triple x _ _) = x

second :: Triple a -> a
second (Triple _ x _) = x

third :: Triple a -> a
third (Triple _ _ x) = x

toList :: Triple a -> [a]
toList (Triple x y z) = [x,y,z]

transform :: (a -> a) -> Triple a -> Triple a
transform f (Triple x y z) = Triple (f x) (f y) (f z)

-- ghci> aPoint
-- Triple 0.1 53.2 12.3
-- ghci> transform (* 3) aPoint
-- Triple 0.30000000000000004 159.60000000000002 36.900000000000006
-- ghci> transform reverse aPerson
-- Triple "drawoH" "spilihP" "tfarcevoL"
-- ghci> import Data.Char
-- ghci> transform toLower initials
-- Triple 'h' 'p' 'l'
-- ghci> toList $ transform toLower initials
-- "hpl"
