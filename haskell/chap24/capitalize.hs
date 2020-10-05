{-# LANGUAGE OverloadedStrings #-}

import System.Environment
import System.IO
import qualified Data.Char as Char
import qualified Data.Text as T
import qualified Data.Text.IO as TI

main:: IO()
main = do
  args <- getArgs
  let fileName1 = head args
  input <- TI.readFile fileName1
  let capitalizedInput = map (\c -> Char.toUpper c) (T.unpack input)
  TI.writeFile fileName1 (T.pack(capitalizedInput))
  putStrLn "done"
