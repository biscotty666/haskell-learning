{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import Data.Semigroup

firstWord :: String
firstWord = "pessimism"

secondWord :: T.Text
secondWord = T.pack firstWord

thirdWord :: String
thirdWord = T.unpack secondWord

sampleInput :: T.Text
sampleInput = "this\nis\ninput"

-- T.lines sampleInput
-- ["this","is","input"]

someText :: T.Text
someText = "Some\n text for\t you"

-- ghci> T.lines someText
-- ["Some"," text for\t you"]
-- ghci> T.words someText
-- ["Some","text","for","you"]

breakText :: T.Text
breakText = "simple"

exampleText :: T.Text
exampleText = "This is simple to do."

-- ghci> T.splitOn breakText exampleText
-- ["This is "," to do."]

combinedTextMonoid :: T.Text
combinedTextMonoid = mconcat ["some"," ","text"]

combinedTextSemigroup :: T.Text
combinedTextSemigroup = "some" <> " " <> "text"
