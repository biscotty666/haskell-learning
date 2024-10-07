import Lib
import Data.Char (isPunctuation)
import Data.Text as T
import Test.QuickCheck
import Test.QuickCheck.Instances

prop_punctuationInvariant :: T.Text -> Bool
prop_punctuationInvariant text = preprocess text == preprocess noPuncText
    where noPuncText = T.filter (not . isPunctuation) text

-- assert :: Bool -> String -> String -> IO ()
-- assert test passStatement failStatment = if test
                                         -- then putStrLn passStatement
                                         -- else putStrLn failStatment

main :: IO ()
main = do
    putStrLn "Running tests..."
    -- assert (isPalindrome "racecar!") "passed 'racecar!'" "FAILED 'racecar!'"
    -- assert (isPalindrome "racecar.") "passed 'racecar.'" "FAILED 'racecar.'"
    -- assert ((not . isPalindrome) "cat") "passed 'cat'" "FAILED 'cat'"
    -- quickCheck prop_punctuationInvariant
    quickCheckWith stdArgs { maxSuccess = 1000 } prop_punctuationInvariant
    putStrLn "done!"
