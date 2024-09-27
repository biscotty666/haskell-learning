data FourLetterAlphabet = L1 | L2 | L3 | L4 deriving (Show,Enum,Bounded)

-- It’s important to notice the type classes you derive and why:
-- You add deriving Show to your alphabet to make it easier to
--      work with this type in GHCi.
-- You add deriving Enum because this will allow you to
--      automatically convert your data constructors to type
--      Int. Being able to convert a letter into an Int allows you
--      to use simple math to rotate out letters. You can
--      use fromEnum to convert your letters to Ints, and
--      toEnum to take Ints and turn them back into letters.
-- Finally, you add deriving Bounded because it provides maxBound
--      and minBound values that will help you know how far you
--      have to cycle.
--
-- The rotN algorithm
-- Here’s how your rotN function is going to work:
-- 1 You pass in the size of your alphabet and a letter you want to rotate.
-- 2 You use the div function to find the middle. Remember, div is different from / in that it divides Ints into whole-valued Ints: although 4 `div` 2 is 2, 5 `div` 2 is also 2. The result of your div function indicates how far you want to rotate your letter.
-- 3 To rotate, you add half of your alphabet size to the Int value of your letter (as an Enum). Of course, for half of your Enum values, adding half the size of the alphabet will give you an Int outside the bounds of your enum. To solve this, you modulo your offset by the alphabet size.
-- 4 Finally, you use toEnum to convert this Int representation of your letter back into an instance of the letter’s type.

rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN alphabetSize c = toEnum rotation
    where halfAlphabet = alphabetSize `div` 2
          offset = fromEnum c + halfAlphabet
          rotation = offset `mod` alphabetSize

largestCharNumber :: Int
largestCharNumber = fromEnum (maxBound :: Char)

rotChar :: Char -> Char
rotChar charToEncrypt = rotN sizeOfAlphabet charToEncrypt
    where sizeOfAlphabet = 1 + fromEnum (maxBound :: Char)

message :: [FourLetterAlphabet]
message = [L1,L3,L4,L1,L2]

fourLetterAlphabetEncoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
fourLetterAlphabetEncoder vals = map rot41 vals
    where alphaSize = 1 + fromEnum (maxBound :: FourLetterAlphabet)
          rot41 = rotN alphaSize

rotNdecoder :: (Bounded a, Enum a) => Int -> a -> a
rotNdecoder n c = toEnum rotation
    where halfN = n `div` 2
          offset = if even n
                   then fromEnum c + halfN
                   else 1 + fromEnum c + halfN
          rotation = offset `mod` n

data ThreeLetterAlphabet = Alpha
                         | Beta
                         | Kappa deriving (Show,Enum,Bounded)

threeLetterMessage :: [ThreeLetterAlphabet]
threeLetterMessage = [Alpha,Alpha,Beta,Alpha,Kappa]

threeLetterEncoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterEncoder vals = map rot3l vals
    where alphaSize = 1 + fromEnum (maxBound :: ThreeLetterAlphabet)
          rot3l = rotN alphaSize

threeLetterDecoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterDecoder vals = map rot3ldecoder vals
    where alphaSize = 1 + fromEnum (maxBound :: ThreeLetterAlphabet)
          rot3ldecoder = rotNdecoder alphaSize

rotEncoder :: String -> String
rotEncoder text = map rotChar text
    where alphaSize = 1 + fromEnum (maxBound :: Char)
          rotChar = rotN alphaSize

rotDecoder :: String -> String
rotDecoder text = map rotCharDecoder text
    where alphaSize = 1 + fromEnum (maxBound :: Char)
          rotCharDecoder = rotNdecoder alphaSize

xorBool :: Bool -> Bool -> Bool
xorBool value1 value2 = (value1 || value2) && (not (value1 && value2))

xorPair :: (Bool,Bool) -> Bool
xorPair (v1,v2) = xorBool v1 v2

xor :: [Bool] -> [Bool] -> [Bool]
xor list1 list2 = map xorPair (zip list1 list2)

type Bits = [Bool]

-- First convert char to int, then convert the base 10
-- int to binary (bits)
--
-- This produces the output in reverse
intToBits' :: Int -> Bits
intToBits' 0 = [False]
intToBits' 1 = [True]
intToBits' n = if (remainder == 0)
               then False : intToBits' nextVal
               else True : intToBits' nextVal
    where remainder = n `mod` 2
          nextVal = n `div` 2

maxBits :: Int
maxBits = length (intToBits' maxBound)

intToBits :: Int -> Bits
intToBits n = leadingFalses ++ reversedBits
    where reversedBits = reverse (intToBits' (fromEnum n))
          missingBits = maxBits - (length reversedBits)
          leadingFalses = take missingBits (cycle [False])

charToBits :: Char -> Bits
charToBits char = intToBits (fromEnum char)

bitsToInt :: Bits -> Int
bitsToInt bits = sum (map (\x -> 2^(snd x)) trueLocations)
    where size = length bits
          indices = [size-1,size-2 .. 0]
          trueLocations = filter (\x -> fst x == True) (zip bits indices)

bitsToChar :: Bits -> Char
bitsToChar bits = toEnum (bitsToInt bits)

myPad :: String
myPad = "Shhhhhh"

myPlainText :: String
myPlainText = "Haskell"

applyOTP' :: String -> String -> [Bits]
applyOTP' pad plaintext = map (\pair ->
                                (fst pair) `xor` (snd pair))
                              (zip padBits plaintextBits)
    where padBits = map charToBits pad
          plaintextBits = map charToBits plaintext

applyOTP :: String -> String -> String
applyOTP pad plaintext = map bitsToChar bitList
    where bitList = applyOTP' pad plaintext

encoderDecoder :: String -> String
encoderDecoder = applyOTP myPad

class Cipher a where
    encode :: a -> String -> String
    decode :: a -> String -> String

data Rot = Rot

instance Cipher Rot where
    encode Rot text = rotEncoder text
    decode Rot text = rotDecoder text

data OneTimePad = OTP String

instance Cipher OneTimePad where
    encode (OTP pad) text = applyOTP pad text
    decode (OTP pad) text = applyOTP pad text

myOTP :: OneTimePad
myOTP = OTP (cycle [minBound .. maxBound])

-- A linear congruential Pseudo-random number generator (PRNG)
prng :: Int -> Int -> Int -> Int -> Int
prng a b maxNumber seed = (a*seed + b) `mod` maxNumber

examplePRNG :: Int -> Int
examplePRNG = prng 1337 7 100
