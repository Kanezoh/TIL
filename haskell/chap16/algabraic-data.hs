-- 直積型 複数の型を論理積で組み合わせる型
data AuthorName = AuthorName String String
-- data Book = Book AuthorName String String Int Double

-- レコード構文
-- data AuthorName = AuthorName {
--     lastName :: String
--   , firstName :: String
-- }
-- data Book = Book {
--     author :: AuthorName
--   , isbn :: String
--   , title :: String
--   , year :: Int
--   , price :: Double
-- }

-- 直和型 複数の型をORで組み合わせる

-- 一般的な直和型のBool
data Bool = False | True

-- 直和型を使ってミドルネームの有無、2つのコンストラクタを定義できる例
type FirstName = String
type LastName = String
type MiddleName = String

data Name = Name FirstName LastName
            | NameWithMiddle FirstName MiddleName LastName
            | TwoInitialsWithLast Char Char LastName

-- Author または ArtistとなるCreator型
newtype Author = Author Name
data Artist = Person Name | Band String
data Creator = AuthorCreator Author | ArtistCreator Artist

hploveCraft :: Creator
hploveCraft = AuthorCreator (Author (TwoInitialsWithLast 'H' 'P' "Lovecraft"))

-- ブックストアプログラムを作成
-- 本
data Book = Book {
    author    :: Creator
  , isbn      :: String
  , bookTitle :: String
  , bookYear  :: Int
  , bookPrice :: Double
}
-- レコード
data VinylRecord = VinylRecord {
    artist      :: Creator
  , recordTitle :: String
  , recordYear  :: String
  , recordPrice :: Double
}
-- おもちゃ
data CollectibleToy = CollectibleToy {
    name        :: String
  , description :: String
  , toyPrice    :: Double
}
-- パンフレット
data Pamphlet = Pamphlet {
    title       :: String
  , description :: String
  , contact     :: String
}

-- 商品を表すデータ型
data StoreItem = BookItem Book
                 | RecordItem VinylRecord
                 | ToyItem CollectibleToy
                 | PamphletItem Pamphlet

-- 全てのItemのpriceを取得する関数
price :: StoreItem -> Double
price (BookItem book) = bookPrice book
price (RecordItem record) = recordPrice record
price (ToyItem toy) = toyPrice toy
price (PamphletItem Pamphlet) = 0

madeby :: StoreItem -> String
madeby (BookItem book) = show (author book)
madeby (RecordItem record) = show (artist record)

-- 図形
type Radius = Double
type Height = Double
type Width  = Double

data Shape = Circle Radius | Square Height Width | Rectangle Height Width deriving Show
perimeter :: Shape -> Double
perimeter (Circle r) = 2 * pi * r
perimeter (Square h) = 4 * h
perimeter (Rectangle h w) = 2 * h + 2 * w 

area :: Shape -> Double
area (Circle r) = r^2 * pi
area (Square h) = h^2
area (Rectangle h w) = h * w
