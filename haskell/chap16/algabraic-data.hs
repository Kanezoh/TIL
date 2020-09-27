-- 直積型 複数の型を論理積で組み合わせる型
data AuthorName = AuthorName String String
data Book = Book AuthorName String String Int Double

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
