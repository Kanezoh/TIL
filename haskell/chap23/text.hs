{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T

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
