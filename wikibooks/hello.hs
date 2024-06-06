main = do
  putStrLn "Hello, everybody!"
  putStrLn ("Some odd numbers: " ++ show (filter odd [10..20]))
