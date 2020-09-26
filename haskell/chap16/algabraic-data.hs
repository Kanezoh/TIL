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
