import System.IO

-- ファイルを開くための関数
-- openFile :: FilePath -> IOMode -> IO Handle
-- type FilePath = String
-- data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode

main :: IO()
-- 読み込み、書き込みを行うコード例
--main = do
--  helloFile <- openFile "hello.txt" ReadMode
--  firstLine <- hGetLine helloFile
--  putStrLn firstLine
--  secondLine <- hGetLine helloFile
--  goodbyeFile <- openFile "goodbye.txt" WriteMode
--  hPutStrLn goodbyeFile secondLine
--  hClose helloFile
--  hClose goodbyeFile
--  putStrLn "done"

main = do
  helloFile <- openFile "hello.txt" ReadMode
  hasLine <- hIsEOF helloFile
  firstLine <- if not hasLine
               then hGetLine helloFile
               else return "empty"
  hClose helloFile
  helloFile <- openFile "hello.txt" ReadWriteMode
  hasSecondLine <- hIsEOF helloFile
  if not hasSecondLine
  then putStrLn "not empty"
  else hPutStrLn helloFile "Hello"
  putStrLn "done"
