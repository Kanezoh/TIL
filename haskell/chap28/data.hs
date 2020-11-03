-- Applicativeはデータを作成するときにデータがMaybeやIOなどの
-- コンテキストに含まれる場合に使える
data User = User { name    :: String
                 , gamerId :: Int
                 , score   :: Int } deriving Show

serverUsername :: Maybe String
serverUsername = Just "sue"

serverGamerId :: Maybe Int
serverGamerId = Just 1337

serverScore :: Maybe Int
serverScore = Just 9001

-- User <$> serverUsername <*> serverGamerId <*> serverScore

readInt :: IO Int
readInt = read <$> getLine

main :: IO()
main = do
  putStrLn "Enter a userame, gamerId, score"
  user <- User <$> getLine <*> readInt <*> readInt
  print user
