module Main where
import qualified Palindrome

isPalindrome :: String -> Bool
isPalindrome text = text == reverse text

main :: IO ()
main = do
    print "Enter a word to check if it is a palindrome:"
    text <- getLine
    let response  = if Palindrome.isPalindrome text
                    then "It is!"
                    else "It isn't"
    print response
