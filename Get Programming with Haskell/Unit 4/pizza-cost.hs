areaGivenDiameter :: Double -> Double
areaGivenDiameter size = pi * (size / 2)^2

type Pizza = (Double,Double)

costPerInch :: Pizza -> Double
costPerInch (size,cost) = cost / areaGivenDiameter size

comparePizzas :: Pizza -> Pizza -> Pizza
comparePizzas p1 p2 = if costP1 < costP2
                      then p1
                      else p2
    where costP1 = costPerInch p1
          costP2 = costPerInch p2

describePizza :: Pizza -> String
describePizza (size,cost) = "The " ++ show size ++ "pizza is cheaper at " ++
                                show costSqInch ++ " per square inch"
    where costSqInch = costPerInch (size,cost)

main :: IO ()
main = do
    putStrLn "What is the size of the first pizza?"
    size1 <- getLine
    putStrLn "What is the cost of the first pizza?"
    cost1 <- getLine
    putStrLn "What is the size of the second pizza?"
    size2 <- getLine
    putStrLn "What is the cost of the second pizza?"
    cost2 <- getLine
    let pizza1 = (read size1, read cost1)
    let pizza2 = (read size2, read cost2)
    let betterPizza = comparePizzas pizza1 pizza2
    putStrLn (describePizza betterPizza)
