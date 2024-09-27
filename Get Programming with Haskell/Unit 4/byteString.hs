{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

sampleBytes :: B.ByteString
sampleBytes = "Hello!"

-- This produces an error:
--
-- sampleString :: String
-- sampleString = B.unpack sampleBytes
-- Couldn't match type ‘GHC.Word.Word8’ with ‘Char’
      -- Expected: String
        -- Actual: [GHC.Word.Word8]

sampleString :: String
sampleString = BC.unpack sampleBytes

bcInt :: BC.ByteString
bcInt = "6"

bcToInt :: BC.ByteString -> Int
bcToInt = read . BC.unpack
