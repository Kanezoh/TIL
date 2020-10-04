import System.Environment
import Control.Monad

main :: IO()
--main = do
--  -- getArgs :: IO [String]
--  args <- getArgs
--  -- mapM_ MonadのコンテキストでListを扱うための関数mapM
--  -- _ をつけると結果を切り捨てる、これがないとmainが期待するIO()に対してリストが返されてコンパイルエラー
--  mapM_ putStrLn args

main = do
  args <- getArgs
  let linesToRead = if length args > 0
                    then read (head args)
                    else 0 :: Int
  -- replicateM 繰り返し
  numbers <- replicateM linesToRead getLine
  let ints = map read numbers :: [Int]
  -- print = putStrLn + show
  print (sum ints)

-- クイックチェック1
threeMain :: IO()
threeMain = do
  vals <- mapM (\ _ -> getLine) [1,2,3]
  mapM_ putStrLn vals

-- クイックチェック2
myReplicateM :: Monad m => Int -> m a -> m [a]
myReplicateM count action = mapM (\ _ -> action) [1..count]

