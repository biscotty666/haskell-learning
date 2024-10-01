-- create a parts database
partsDB :: Map.Map Int RobotPart
partsDB = Map.fromList keyVals
    where keys = [1,2,3]
          vals = [leftArm,rightArm,robotHead]
          keyVals = zip keys vals

-- convert to HTML
htmlPartsDB :: Map.Map Int Html
htmlPartsDB = renderHtml <$> partsDB
-- ghci> Map.lookup 1 htmlPartsDB
-- Just "<h2>left arm</h2><p><h3>desc</h3>left arm for face punching</p><p><h3>cost</h3>1000.0</p><p><h3>count</h3>3</p>"

-- simulate an IO type robot part
leftArmIO :: IO RobotPart
leftArmIO = return leftArm

htmlSnippet :: IO Html
htmlSnippet = renderHtml <$> leftArmIO


-- create an HTML representation of a part
partVal :: Maybe RobotPart
partVal = Map.lookup 1 partsDB

partHtml :: Maybe Html
partHtml = renderHtml <$> partVal

-- convert a parts list to HTML
allParts :: [RobotPart]
allParts = map snd (Map.toList partsDB)

-- List is an instance of Functor, so...
allPartsHtml :: [Html]
allPartsHtml = renderHtml <$> allParts
-- thes is identical to
-- allPartsHtml :: [Html]
-- allPartsHtml = map renderHtml allParts

data RobotPart = RobotPart
    { name :: String
    , description :: String
    , cost :: Double
    , count :: Int
    } deriving Show

leftArm :: RobotPart
leftArm = RobotPart
    { name = "left arm"
    , description = "left arm for face punching"
    , cost = 1000.00
    , count = 3
    }

rightArm :: RobotPart
rightArm = RobotPart
    { name = "right arm"
    , description = "right arm for hand gestures"
    , cost = 1025.00
    , count = 5
    }

robotHead :: RobotPart
robotHead = RobotPart
    { name = "left arm"
    , description = "Pissed off head"
    , cost = 5092.25
    , count = 2
    }

type Html = String

renderHtml :: RobotPart -> Html
renderHtml part = mconcat [ "<h2>",partName, "</h2>"
                          , "<p><h3>desc</h3>", partDesc
                          , "</p><p><h3>cost</h3>", partCost
                          , "</p><p><h3>count</h3>", partCount
                          , "</p>"]
    where partName = name part
          partDesc = description part
          partCost = show (cost part)
          partCount = show (count part)
