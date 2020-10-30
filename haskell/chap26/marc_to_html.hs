{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as E
import Data.Maybe

type Author = T.Text
type Title  = T.Text
data Book = Book { author :: Author, title :: Title } deriving Show
type Html = T.Text

bookToHtml :: Book -> Html
bookToHtml book = mconcat ["<p>\n", titleInTags, authorInTags, "</p>\n"]
  where titleInTags = mconcat ["<strong>", (title book), "</strong>\n"]
        authorInTags = mconcat ["<em>", (author book), "</em>\n"]

booksToHtml :: [Book] -> Html
booksToHtml books = mconcat ["<html>\n"
                            ,"<head><title>books</title>"
                            ,"<meta charset='utf-8'/>"
                            ,"</head>\n"
                            ,"<body>\n"
                            ,booksHtml
                            ,"\n</body>\n"
                            ,"</html>"]
  where booksHtml = (mconcat . (map bookToHtml)) books

book1 :: Book
book1 = Book { title = "The Conspiracy Against the Human Race"
               ,author = "Ligotti, Thomas"}
book2 :: Book
book2 = Book { title = "A Short History of Decay"
               ,author = "Cloran, Emil"}
book3 :: Book
book3 = Book { title = "The Tears of Eros"
               ,author = "Bataille, Georges"}

myBooks :: [Book]
myBooks = [book1, book2, book3]

type MarcRecordRaw = B.ByteString
type MarcLeaderRaw = B.ByteString

leaderLength :: Int
leaderLength = 24
-- MARCレコードからリーダーを取得(最初の24バイトがリーダー)
getLeader :: MarcRecordRaw -> MarcLeaderRaw
getLeader record = B.take leaderLength record

rawToInt :: B.ByteString -> Int
rawToInt = read . T.unpack . E.decodeUtf8
-- リーダーからレコードの長さを取得(最初の5バイトがレコードの長さ)
getRecordLength :: MarcLeaderRaw -> Int
getRecordLength leader = rawToInt (B.take 5 leader)
-- marcレコードから先頭1件分のデータと残りのデータに分割
nextAndRest :: B.ByteString -> (MarcRecordRaw, B.ByteString)
nextAndRest marcStream = B.splitAt recordLength marcStream
  where recordLength = getRecordLength marcStream

allRecords :: B.ByteString -> [MarcRecordRaw]
allRecords marcStream = if marcStream == B.empty
                        then []
                        else next : allRecords rest
  where (next, rest) = nextAndRest marcStream

type MarcDirectoryRaw = B.ByteString
-- ベースアドレスの情報はリーダーの12~16番目のバイト
getBaseAddress :: MarcLeaderRaw -> Int
getBaseAddress leader = rawToInt (B.take 5 remainder)
  where remainder = B.drop 12 leader
-- リーダーが終わる場所〜ベースアドレスの位置までがディレクトリ
getDirectoryLength :: MarcLeaderRaw -> Int
getDirectoryLength leader = getBaseAddress leader - (leaderLength - 1)
-- ディレクトリを取得
getDirectory :: MarcLeaderRaw -> MarcDirectoryRaw
getDirectory record = B.take directoryLength afterLeader
  where directoryLength = getDirectoryLength record
        afterLeader     = B.drop leaderLength record

type MarcDirectoryEntryRaw = B.ByteString
dirEntryLength :: Int
dirEntryLength = 12
-- ディレクトリを12バイトごとのエントリに区切る
splitDirectory :: MarcDirectoryRaw -> [MarcDirectoryEntryRaw]
splitDirectory directory = if directory == B.empty
                           then []
                           else nextEntry : splitDirectory restEntries
  where (nextEntry, restEntries) = B.splitAt dirEntryLength directory

data FieldMetaData = FieldMetaData { tag         :: T.Text
                                    ,fieldLength :: Int
                                    ,fieldStart  :: Int } deriving Show
makeFieldMetaData :: MarcDirectoryEntryRaw -> FieldMetaData
makeFieldMetaData entry = FieldMetaData textTag theLength theStart
  where (theTag, rest) = B.splitAt 3 entry
        textTag = E.decodeUtf8 theTag
        (rawLength, rawStart) = B.splitAt 4 rest
        theLength = rawToInt rawLength
        theStart  = rawToInt rawStart

getFieldMetaData :: [MarcDirectoryEntryRaw] -> [FieldMetaData]
getFieldMetaData rawEntries = map makeFieldMetaData rawEntries

type FieldText = T.Text

getTextField :: MarcRecordRaw -> FieldMetaData -> FieldText
getTextField record fieldMetaData = E.decodeUtf8 byteStringValue
  where recordLength = getRecordLength record
        baseAddress  = getBaseAddress  record
        baseRecord   = B.drop baseAddress record
        baseAtEntry  = B.drop (fieldStart fieldMetaData) baseRecord
        byteStringValue = B.take (fieldLength fieldMetaData) baseAtEntry

fieldDelimiter :: Char
fieldDelimiter = toEnum 31

titleTag :: T.Text
titleTag = "245"
titleSubField :: Char
titleSubField = 'a'
authorTag :: T.Text
authorTag = "100"
authorSubField :: Char
authorSubField = 'a'

lookupFieldMetaData :: T.Text -> MarcRecordRaw -> Maybe FieldMetaData
lookupFieldMetaData aTag record = if length results < 1
                                  then Nothing
                                  else Just (head results)
  where metaData = (getFieldMetaData . splitDirectory . getDirectory) record
        results  = filter ((== aTag) . tag) metaData

lookupSubField :: (Maybe FieldMetaData) -> Char -> MarcRecordRaw -> Maybe T.Text
lookupSubField Nothing subfield record = Nothing
lookupSubField (Just fieldMetaData) subfield record = if results == []
                                                      then Nothing
                                                      else Just ((T.drop 1 . head) results)
  where rawField  = getTextField record fieldMetaData
        subfields = T.split (== fieldDelimiter) rawField
        results   = filter ((== subfield) . T.head) subfields

lookupValue :: T.Text -> Char -> MarcRecordRaw -> Maybe T.Text
lookupValue aTag subfield record = lookupSubField entryMetaData subfield record
  where entryMetaData = lookupFieldMetaData aTag record

lookupTitle :: MarcRecordRaw -> Maybe Title
lookupTitle = lookupValue titleTag titleSubField

lookupAuthor :: MarcRecordRaw -> Maybe Author
lookupAuthor = lookupValue authorTag authorSubField

marcToPairs :: B.ByteString -> [(Maybe Title, Maybe Author)]
marcToPairs marcStream = zip titles authors
  where records = allRecords marcStream
        titles  = map lookupTitle records
        authors = map lookupAuthor records

pairsToBooks :: [(Maybe Title, Maybe Author)] -> [Book]
pairsToBooks pairs = map (\(title, author) -> Book {
   title  = fromJust title
  ,author = fromJust author
}) justPairs
  where justPairs = filter (\(title, author) -> isJust title && isJust author) pairs

processRecords :: Int -> B.ByteString -> Html
processRecords n = booksToHtml . pairsToBooks . (take n) . marcToPairs

main :: IO()
main = do
  marcData <- B.readFile "sample.mrc"
  let processed = processRecords 500 marcData
  TIO.writeFile "books.html" processed
