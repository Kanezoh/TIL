import System.Environment
import System.Random
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

intToChar :: Int -> Char
intToChar int = toEnum safeInt
  where safeInt = int `mod` 255

intToBC :: Int -> BC.ByteString
intToBC int = BC.pack [intToChar int]

replaceByte :: Int -> Int -> BC.ByteString -> BC.ByteString
-- 新しいByteStringは前の部分と後ろの部分をnewCharで繋いだもの
replaceByte loc charVal bytes = mconcat [before, newChar, after]
  -- パターンマッチングを使って2つの値を2つの変数に一度に割り当てる
  where (before, rest) = BC.splitAt loc bytes
        after = BC.drop 1 rest
        newChar = intToBC charVal

-- ランダムで指定したバイト文字を入れ替える
randomReplaceByte :: BC.ByteString -> IO BC.ByteString
randomReplaceByte bytes = do
  let bytesLength = BC.length bytes
  location <- randomRIO (1, bytesLength)
  charVal  <- randomRIO (0, 255)
  return (replaceByte location charVal bytes)

-- ファイルの一部を並べ替える
sortSection :: Int -> Int -> BC.ByteString -> BC.ByteString
sortSection start size bytes = mconcat [before, changed, after]
  where (before, rest)  = BC.splitAt start bytes
        (target, after) = BC.splitAt size rest
        changed         = BC.reverse (BC.sort target)

-- sortSection関数をランダム化する
randomSortSection :: BC.ByteString -> IO BC.ByteString
randomSortSection bytes = do
  -- 並べ替えるセクションのサイズを適当に選択
  let sectionSize = 25
  let bytesLength = BC.length bytes
  -- randomRIOを使って並べ替えを開始する位置を決定
  start <- randomRIO (0, bytesLength - sectionSize)
  return (sortSection start sectionSize bytes)

main :: IO()
main = do
  args <- getArgs
  let fileName = head args
  imageFile <- BC.readFile fileName
  glitched <- randomSortSection imageFile
  let glitchedFileName = mconcat ["glitched_", fileName]
  BC.writeFile glitchedFileName glitched
  print "all done"

-- クイックチェック3
randomChar :: IO Char
randomChar = do
  randomInt <- randomRIO(0, 255)
  return (toEnum randomInt)
