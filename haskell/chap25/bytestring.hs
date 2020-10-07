{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

sampleBytes :: B.ByteString
sampleBytes = "Hello"

sampleString :: String
sampleString = BC.unpack sampleBytes

-- クイックチェック1
bcInt :: BC.ByteString
bcInt = "6"

toInt :: Int
toInt = read (BC.unpack bcInt) :: Int
