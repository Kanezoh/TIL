import System.Environment

main :: IO()
main = do
  -- getArgs :: IO [String]
  args <- getArgs
  -- mapM_ MonadのコンテキストでListを扱うための関数mapM
  -- _ をつけると結果を切り捨てる、これがないとmainが期待するIO()に対してリストが返されてコンパイルエラー
  mapM_ putStrLn args

threeMain :: IO()
threeMain = do
  vals <- mapM (\ _ -> getLine) [1,2,3]
  mapM_ putStrLn vals

