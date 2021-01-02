module Main where
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
instance FromJSON ErrorMessage where
  parseJSON (Object v) = ErrorMessage <$> v .: "message" <*> v .: "error"

exampleMessage :: Maybe T.Text
exampleMessage = Just "opps"
exampleError :: Maybe Int
exampleError = Just 123

main :: IO ()
main = print "hi"
