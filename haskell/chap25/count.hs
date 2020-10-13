import System.Environment
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

getCounts :: T.Text -> (Int, Int, Int)
getCounts input = (charCount, wordCount, lineCount)
  where charCount = T.length input
        wordCount = (length . T.words) input
        lineCount = (length . T.lines) input

main :: IO()
main = do
  args <- getArgs
  let fileName = head args
  inputText <- TI.readFile fileName
  let countChar = T.length inputText
  inputByteString <- BC.readFile fileName
  let countByte = BC.length inputByteString

  putStrLn (show countChar)
  putStrLn (show countByte)
