module Main where

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.Time

data Tool = Tool { toolId :: Int
                 , name :: String
                 , description :: String
                 , lastReturned :: Day
                 , timesBorrowed :: Int }
data User = User { userId :: Int
                 , userName :: String }                 

instance Show User where
  show user = mconcat [ show $ userId user, ".) ", userName user]

instance Show Tool where
  show tool = mconcat [ show $ toolId tool, ".) "
                      , name tool
                      , "\n description: "
                      , description tool
                      , "\n last returned: "
                      , show $ lastReturned tool
                      , "\n times borrowed: "
                      , show $ timesBorrowed tool
                      , "\n"]
addUser :: String -> IO ()
addUser userName = withConn "tools.db" $ \conn -> do
  execute conn "INSERT INTO users (username) VALUES (?)" (Only userName)
  print "user added"

withConn :: String -> (Connection -> IO()) -> IO()
withConn dbName action = do
  conn <- open dbName
  action conn
  close conn

checkout :: Int -> Int -> IO()
checkout userId toolId = withConn "tools.db" $ \conn -> do
  execute conn "INSERT INTO checkedout (user_id,tool_id) VALUES (?,?)" (userId,toolId)

-- SQLのレコードをHaskellのデータ型に変換する
instance FromRow User where
  fromRow = User <$> field <*> field
instance FromRow Tool where
  fromRow = Tool <$> field <*> field <*> field <*> field <*> field

printUsers :: IO()
printUsers = withConn "tools.db" $ \conn -> do
  resp <- query_ conn "SELECT * FROM users;" :: IO [User]
  mapM_ print resp

printToolQuery :: Query -> IO()
printToolQuery q = withConn "tools.db" $ \conn -> do
  resp <- query_ conn q :: IO [Tool]
  mapM_ print resp

printTools :: IO()
printTools = printToolQuery "SELECT * FROM tools;"

printAvailable :: IO()
printAvailable = printToolQuery $
  mconcat [ "select * from tools "
          , "where id not in "
          , "(select tool_id from checkedout);"]

printCheckedout :: IO()
printCheckedout = printToolQuery $
  mconcat [ "select * from tools "
         , "where id in "
         , "(select tool_id from checkedout);"]

main :: IO ()
main = print "db-lesson"
