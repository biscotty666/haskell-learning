main = do
  putStrLn "Please enter your name: "
  name <- getLine
  putStrLn("Hello, " ++ name ++ ", how are you?")

calcTriangleArea = do
  putStrLn "Enter the base: "
  base <- getLine
  putStrLn "\nEnter the height: "
  height <- getLine
  putStrLn ("\nThe area is " ++ show (0.5 * read base * read height))

doGuessing num = do
   putStrLn "Enter your guess:"
   guess <- getLine
   if (read guess) < num
     then do putStrLn "Too low!"
             doGuessing num
     else if (read guess) > num
            then do putStrLn "Too high!"
                    doGuessing num
            else putStrLn "You Win!"
