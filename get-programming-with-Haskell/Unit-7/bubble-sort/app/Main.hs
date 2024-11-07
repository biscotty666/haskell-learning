module Main (main) where

import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad
import Control.Monad.ST

-- Comparing lists to arrays
aLargeList :: [Int]
aLargeList = [1..10000000]

aLargeArray :: UArray Int Int
aLargeArray = array (0,9999999) []

zeroIndexArray :: UArray Int Bool
zeroIndexArray = array (0,9) [(3,True)]

oneIndexArray :: UArray Int Bool
oneIndexArray = array (1,10) $ zip [1..10] $ cycle [True]

beansInBuckets :: UArray Int Int
beansInBuckets = array (0,3) $ zip [0..3] $ cycle [0]

updatedBiB :: UArray Int Int
updatedBiB = beansInBuckets // [(1,5),(3,6)]

-- array (0,3) [(0,0),(1,5),(2,0),(3,6)]
-- ghci> accum (+) updatedBiB $ zip [0..3] $ cycle [2]
-- array (0,3) [(0,2),(1,7),(2,2),(3,8)]
-- ghci> accum (+) updatedBiB $ zip [0..3] $ [1,2,3,4]
-- array (0,3) [(0,1),(1,7),(2,3),(3,10)]
-- ghci> accum (*) updatedBiB $ zip [0..3] $ [1,2,3,4]
-- array (0,3) [(0,0),(1,10),(2,0),(3,24)]

-- STUarrays allow for mutability
--
-- Turning lists into STUarrays

listToSTUArray :: [Int] -> ST s (STUArray s Int Int)
listToSTUArray vals = do
    let end = length vals - 1
    myArray <- newArray (0,end) 0
    forM_ [9 .. end] $ \i -> do
      let val = vals !! i
      writeArray myArray i val
    return myArray

-- ghci> listToSTUArray [1,2,3]
-- <<ST action>>

listToUArray' :: [Int] -> UArray Int Int
listToUArray' vals = runSTUArray $ listToSTUArray vals

-- more commonly combined as one function
listToUArray :: [Int] -> UArray Int Int
listToUArray vals = runSTUArray $ do
    let end = length vals - 1
    myArray <- newArray (0,end) 0
    forM_ [9 .. end] $ \i -> do
      let val = vals !! i
      writeArray myArray i val
    return myArray

-- ghci> listToUArray [1,2,3]
-- array (0,2) [(0,0),(1,0),(2,0)]

{-
    Bubble Sort

    uses funcions:
    - thaw : unfreeze a UArray to work with it
    - bounds: returns a pair of start and end
    - readArray : reads a stateful value from array
    - when: if without then
-}

myData :: UArray Int Int
myData = listArray (0,5) [7,6,4,8,10,2]

bubbleSort :: UArray Int Int -> UArray Int Int
bubbleSort myArray = runSTUArray $ do
    stArray <- thaw myArray
    let end = (snd . bounds) myArray
    forM_ [1 .. end] $ \i -> do
        forM_ [0 .. (end - i)] $ \j -> do
            val <- readArray stArray j
            nextVal <- readArray stArray (j + 1)
            let outOfOrder = val > nextVal
            when outOfOrder $ do
                writeArray stArray j nextVal
                writeArray stArray (j + 1) val
    return stArray


main :: IO ()
main = putStrLn "bubble, bubble"
