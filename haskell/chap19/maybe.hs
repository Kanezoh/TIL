-- 18章のコード
import qualified Data.Map as Map
import Data.List

data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq, Ord)
organs :: [Organ]
organs = [Heart, Heart, Brain, Spleen, Spleen, Kidney]

ids :: [Int]
ids = [2, 7, 13, 14, 21, 24]

organPairs :: [(Int, Organ)]
organPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

-- Maybe型、欠損値に対処するための型
-- data Maybe a = Nothing | Just a
-- Map.lookup 13 organCatalog => Just Brain
-- Map.lookup 6  organCatalog => Nothing

-- 他言語におけるNULLが存在しない代わりに、欠損値が存在すると予測される値はMaybeで返される
-- 値がMaybeでラップされていることを必然的に意識しなければいけない設計になっている

possibleDrawers :: [Int]
possibleDrawers = [1..50]

getDrawersContent :: [Int] -> Map.Map Int Organ -> [Maybe Organ]
getDrawersContent ids catalog = map getContents ids
  where getContents id = Map.lookup id catalog

availableOrgans :: [Maybe Organ]
availableOrgans = getDrawersContent possibleDrawers organCatalog

countOrgan :: Organ -> [Maybe Organ] -> Int
countOrgan organ available = length (filter (\x -> x == Just organ) available)

emptyDrawers :: [Maybe Organ] -> Int
emptyDrawers organs = length (filter (\x -> x == Nothing) organs) 

-- Nothingかどうかを判定する関数
isSomething :: Maybe Organ -> Bool
isSomething Nothing = False
isSomething _ = True

-- Nothingを除いたOrgans
justTheOrgans :: [Maybe Organ]
justTheOrgans = filter isSomething availableOrgans

-- Justも除きたい
showOrgan :: Maybe Organ -> String
showOrgan (Just organ) = show organ
showOrgan Nothing = ""

-- showOrganをmapで使用
organList :: [String]
organList = map showOrgan justTheOrgans 

-- 見た目をよくするためにカンマを挿入
-- intercalate関数のためにData.Listをインポート

cleanList :: String
cleanList = intercalate ", " organList

-- 練習問題 numOrZero関数を作成
numOrZero :: Maybe Int -> Int
numOrZero (Just int) = int
numOrZero _ = 0
