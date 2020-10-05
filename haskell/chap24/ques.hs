-- 練習問題1
import System.Environment
import System.IO

main :: IO()
main = do
  args <- getArgs
  let fileName1 = head args
  let fileName2 = args !! 1
  input <- readFile fileName1
  writeFile fileName2 input
  putStrLn "done"
