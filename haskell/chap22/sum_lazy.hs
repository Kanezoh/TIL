main :: IO()
main = do
  userInput <- getContents
  mapM_ print userInput

-- クイックチェック3 反転して出力
-- reverseMain :: IO()
-- reverseMain = do
-- userInput <- getContents
--  let reversed = reverse userInput
--  print reversed
