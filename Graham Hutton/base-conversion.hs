import Data.Char

type Bit = Int

bin2int :: [Bit] -> Int
bin2int bits = sum [w*b | (w,b) <- zip weights bits]
                where weights = iterate (*2) 1

-- (1 * a) +(2 * b) +(4 * c) +(8 * d)
-- which can be restructured as follows:
-- (1 * a) +(2 * b) +(4 * c) +(8 * d)
-- =
-- { simplifying 1 * a }
-- a +(2 * b) +(4 * c) +(8 * d)
-- =
-- { factoring out 2 *}
-- a + 2 * (b +(2 * c) +(4 * d))
-- =
-- { factoring out 2 *}
-- a + 2 * (b + 2 * (c +(2 * d)))
-- =
-- { complicating d }
-- a + 2 * (b + 2 * (c + 2 * (d + 2 * 0)))
--
-- The final result shows that converting a list of bits
-- [a, b, c, d] into an integer amounts to replacing each
-- cons by the function that adds its first argument to
-- twice its second argument, and replacing the empty list
-- by zero.
--
bin2int' :: [Bit] -> Int
bin2int' = foldr (\x y -> x + 2*y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

-- extend or shorten to 8 bits
make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id
