{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import Data.Semigroup

-- String→リスト Text→配列
-- 遅延評価をしないのが特徴

-- 相互変換
firstWord :: String
firstWord = "pessimism"

secondWord :: T.Text
secondWord = T.pack firstWord

thirdWord :: String
thirdWord = T.unpack secondWord

-- 以下のコードは何もしていないとエラーになる
myWord :: T.Text
myWord = "dog"

-- 有効にする方法
-- 1. コンパイル時に -XOverloadedStrings オプションを付ける
-- 2. {-# LANGUAGE OverloadedStrings #-} をファイルの先頭に付ける

-- テキストユーティリティ

-- lines 改行文字を取り除く
sampleInput :: T.Text
sampleInput = "this\nis\ninput"

-- T.lines sampleInput
-- => ["this","is","input"]

-- words 改行文字、ホワイトスペース文字を取り除く
someText :: T.Text
someText = "Some\ntext for\tyou"

-- T.words someText
-- => ["Some","text","for","you"]

-- splitOn 
breakText :: T.Text
breakText = "simple"

exampleText :: T.Text
exampleText = "This is simple to do"

-- T.splitOn breakText exampleText
-- ["This is "," to do"]

-- unlines, unwords Textをホワイトベースで結合する

-- T.unlines (T.lines sampleInput)
-- "this\nis\ninput\n"

-- T.unwords(T.words someText)
-- "Some text for you"

-- intercalate splitOnと逆
-- T.intercalate breakText (T.splitOn breakText exampleText)
-- => "This is simple to do"

-- モノイド演算
-- TextはString型のような ++ 演算子で結合できない

combinedTextMonoid :: T.Text
combinedTextMonoid = mconcat ["some", " ", "text"]

combinedTextSemigroup :: T.Text
combinedTextSemigroup = "some" <> " " <> "text"

-- クイックチェック3
myLines text = T.splitOn "\n" text
myUnlines textList = T.intercalate "\n" textList
