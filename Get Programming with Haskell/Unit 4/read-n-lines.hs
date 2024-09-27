import Control.Monad

main :: IO ()
main = do
    args <- getArgs
    let linesToRead = if length args > 0
                      then read (head args)
                      else 0
    numbers <- replicateM linesToRead getLine
    print "sum goes here"
