import System.Environment
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

main :: IO()
main = do
  args <- getArgs
  let fileName = head args
  imageFile <- BC.readFile fileName
  -- TODO returnはあとでバイナリデータを変更する関数に置き換える
  glitched <- return imageFile
  let glitchedFileName = mconcat ["glitched_", fileName]
  BC.writeFile glitchedFileName glitched
  print "all done"
