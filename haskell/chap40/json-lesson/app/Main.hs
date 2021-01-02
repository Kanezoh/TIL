module Main where
import Control.Monad
import Data.Aeson
import Data.Text as T
import Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Char8 as BC
import GHC.Generics

instance FromJSON Book
instance ToJSON Book

data Book = Book { title :: T.Text
                 , author :: T.Text
                 , year :: Int } deriving (Show, Generic)

myBook :: Book
myBook = Book { author="Will Kurt", title="Learn Haskell", year=2017}
myBookJSON :: BC.ByteString
myBookJSON = encode myBook
rawJSON :: BC.ByteString
rawJSON = "{\"author\":\"Emil Ciroan\",\"title\":\"A Short History of Decay\",\"year\":1949}"
bookFromJSON :: Maybe Book
bookFromJSON = decode rawJSON

-- 間違ったJSON、Nothingを返す
wrongJSON :: BC.ByteString
wrongJSON = "{\"writer\":\"Emil Ciroan\",\"title\":\"A Short History of Decay\",\"year\":1949}"
bookFromWrongJSON :: Maybe Book
bookFromWrongJSON = decode wrongJSON

sampleError :: BC.ByteString
sampleError = "{\"message\":\"opps\",\"error\":123}"
-- sampleErrorに対応するデータ型
-- errorという名前のプロパティを作りたいが、組み込み関数のエラーと被る
data ErrorMessage = ErrorMessage { message :: T.Text
                                 , errorCode :: Int
                                 } deriving Show
-- parseJSONを使って記述する
instance FromJSON ErrorMessage where
  parseJSON (Object v) = ErrorMessage <$> v .: "message" <*> v .: "error"

exampleMessage :: Maybe T.Text
exampleMessage = Just "opps"
exampleError :: Maybe Int
exampleError = Just 123

sampleErrorMessage :: Maybe ErrorMessage
sampleErrorMessage = decode sampleError

-- ToJSONも独自に記述する
instance ToJSON ErrorMessage where
  toJSON (ErrorMessage message errorCode) = object [ "message" .= message
                                                    , "error"   .= errorCode
                                                    ]
anErrorMessage :: ErrorMessage
anErrorMessage = ErrorMessage "Everything is okay" 0

-- NOAAのJSONに対応する
data NOAAResult = NOAAResult { uid :: T.Text
                             , mindate :: T.Text
                             , maxdate :: T.Text
                             , name :: T.Text
                             , datacoverage :: Double 
                             , resultId :: T.Text } deriving Show

instance FromJSON NOAAResult where
  parseJSON (Object v) = NOAAResult <$> v .: "uid"
                                    <*> v .: "mindate"
                                    <*> v .: "maxdate"
                                    <*> v .: "name"
                                    <*> v .: "datacoverage"
                                    <*> v .: "id"
-- metadata型
data Resultset = Resultset { offset :: Int
                           , count  :: Int
                           , limit  :: Int } deriving (Show,Generic)                                    
instance FromJSON Resultset
data Metadata = Metadata { resultset :: Resultset } deriving (Show, Generic)
instance FromJSON Metadata
-- レスポンスの型
data NOAAResponse = NOAAResponse { metadata :: Metadata
                                 , results  :: [NOAAResult]
                                 } deriving (Show,Generic)
instance FromJSON NOAAResponse

-- エラーを出力する
printResults :: Maybe [NOAAResult] -> IO()
printResults Nothing = print "error loading data"
printResults (Just results) = do
  forM_ results $ \result -> do
    let dataName = name result
    print dataName

main :: IO ()
main = do
  jsonData <- B.readFile "data.json"
  let noaaResponse = decode jsonData :: Maybe NOAAResponse
  let noaaResults = results <$> noaaResponse
  printResults noaaResults

