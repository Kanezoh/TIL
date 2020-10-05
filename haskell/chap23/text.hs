import qualified Data.Text as T

-- String→リスト Text→配列
-- 遅延評価をしないのが特徴

firstWord :: String
firstWord = "pessimism"

secondWord :: T.Text
secondWord = T.pack firstWord

thirdWord :: String
thirdWord = T.unpack secondWord
