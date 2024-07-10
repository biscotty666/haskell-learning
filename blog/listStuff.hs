safeHead :: [a] -> Maybe a
safeHead list =
    case list of
      []    -> Nothing
      x : _ -> Just x

exactlyTwo :: [a] -> Maybe (a, a)
exactlyTwo list =
    case list of
      [x, y] -> Just (x, y)
      _      -> Nothing

exactlyTwo' :: [a] -> Maybe (a, a)
exactlyTwo' list =
    case list of
      x : y : [] -> Just (x, y)
      _          -> Nothing
