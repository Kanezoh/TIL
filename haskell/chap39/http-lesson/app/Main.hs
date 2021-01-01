module Main where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Network.HTTP.Simple

myToken :: BC.ByteString
myToken = ""
noaaHost :: BC.ByteString
noaaHost = "www.ncdc.noaa.gov"
apiPath :: BC.ByteString
apiPath = "/cdo-Web/api/v2/datasets"

main :: IO ()
main = print "hi"
