increment :: Int -> Int
increment x = x + 1

decrement :: Int -> Int
decrement x = x - 1

add :: Int -> Int -> Int
add x y =
    if y /= 0
       then add (increment x) (decrement y)
       else x

minus :: Int -> Int -> Int
minus x y =
   if y /= 0
      then minus (decrement x ) ( decrement y)
      else x
