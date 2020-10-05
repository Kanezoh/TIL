{-# LANGUAGE OverloadedStrings #-}
--import qualified Data.Text as T
--import qualified Data.Text.IO as TIO
import Data.List
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO

-- 練習問題1
-- helloPerson :: T.Text -> T.Text
-- helloPerson name = mconcat ["Hello", name, "!"]
-- 
-- main :: IO()
-- main = do
--   putStrLn "Hello, What's your name?"
--   name <- getLine
--   let statement = helloPerson (T.pack(name))
--   TIO.putStrLn statement

toInts :: T.Text -> [Int]
toInts = map (read . T.unpack) . T.lines

main :: IO()
main = do
  userInput <- TIO.getContents
  let numbers = toInts userInput
  TIO.putStrLn ((T.pack . show . sum) numbers)
