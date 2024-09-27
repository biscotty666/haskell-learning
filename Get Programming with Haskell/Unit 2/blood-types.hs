type FirstName = String
type LastName = String
type Age = Int
type Height = Int
type PatientName = (String, String)
type MiddlName = String

data Sex = Male | Female

sexInitial :: Sex -> Char
sexInitial Male = 'M'
sexInitial Female = 'F'

data RhType = Pos | Neg
data ABOType = A | B | AB | O
data BloodType = BloodType ABOType RhType

patient1BT :: BloodType
patient1BT = BloodType A Pos

patient2BT :: BloodType
patient2BT = BloodType O Neg

patient3BT :: BloodType
patient3BT = BloodType AB Pos

showRH :: RhType -> String
showRH Pos = "+"
showRH Neg = "-"

showABO :: ABOType -> String
showABO A = "+"
showABO B = "B"
showABO O = "O"
showABO AB = "AB"

showBloodType :: BloodType -> String
showBloodType (BloodType abo rh) = showABO abo ++ showRH rh

canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _ = True
canDonateTo _ (BloodType AB _) = True
canDonateTo (BloodType A _) (BloodType A _) = True
canDonateTo (BloodType B _) (BloodType B _) = True
canDonateTo _ _ = False

patientInfo :: PatientName -> Age -> Height -> String
patientInfo (fname, lname) age height = name ++ " " ++ ageHeight
 where name = lname ++ ", " ++ fname
       ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"

firstName :: PatientName -> String
firstName patient = fst patient

lastName :: PatientName -> String
lastName patient = snd patient

myPatient = ("John", "Doe")

data Name = Name FirstName LastName
          | NameWithMiddle FirstName MiddlName LastName
          deriving (Eq, Show)

instance Ord Name where
    compare (Name f1 l1) (Name f2 l2) = compare (l1,f1) (l2,f2)

names :: [Name]
names = [Name "Emil" "Chioran"
        , Name "Eugene" "Thacker"
        , Name "Friedrich" "Nietzshe"]
showName :: Name -> String
showName (Name f l) = f ++ " " ++ l
showName (NameWithMiddle f m l) = f ++ " " ++ m ++ " " ++ l

name1 = Name "Jerome" "Salinger"
name2 = NameWithMiddle "Jerome" "David" "Salinger"

data Patient = Patient { name :: Name
                       , sex :: Sex
                       , age :: Int
                       , height :: Int
                       , weight :: Int
                       , bloodType :: BloodType }

johnDoe :: Patient
johnDoe = Patient (Name "John" "Doe") Male 30 74 200 (BloodType AB Pos)

janeESmith :: Patient
janeESmith = Patient (NameWithMiddle "Jane" "Elizabeth" "Smith") Female 28 62 140 (BloodType O Neg)

jackieSmith :: Patient
jackieSmith = Patient { name = Name "Jackie" "Smith"
                      , age = 43
                      , sex = Female
                      , height = 62
                      , weight = 115
                      , bloodType = BloodType O Neg }

-- ghci> height jackieSmith
-- 62
-- ghci> showBloodType (bloodType jackieSmith)
-- "O-"

jackieSmithUpdated = jackieSmith { age = 44 }
