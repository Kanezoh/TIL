import Data.List

ifEven myFunction x = if even x
                      then myFunction x
                      else x
inc n = n + 1
double n = n * 2
square n = n^2

-- クイックチェック4-1
-- xを３乗してifEvenに渡すラムダ関数を作成しろ
--ifEven (\x -> x^3)x


names = [("Ian","Curtis"),
         ("Bernard","Sumner"),
         ("Peter","Hook"),
         ("Stephen","Morris")]

--compareLastName name1 name2 = if lastName1 > lastName2
--                               then GT
--                               else if lastName2 > lastName1
--                                    then LT
--                                    else EQ
--  where lastName1 = snd name1
--        lastName2 = snd name2
--クイックチェック4-2
--compareLastName関数ではラストネームが同じでファーストネームが同じ場合に対処できない、修正せよ  

compareLastName name1 name2 = if lastName1 > lastName2
                               then GT
                               else if lastName2 > lastName1
                                    then LT
                                    else if firstName1 > firstName2
                                      then GT
                                      else if firstName2 > firstName1
                                        then LT
                                        else EQ
  where lastName1 = snd name1
        lastName2 = snd name2
        firstName1 = fst name1
        firstName2 = fst name2

hokkaidoOffice name = if lastName < "L"
                      then nameText ++ " - 北海道札幌市"
                      else nameText ++ " - 北海道釧路市"
  where lastName = snd name
        nameText = (fst name) ++ " " ++ (snd name)

osakaOffice name = nameText ++ " : 大阪府大阪市"
  where nameText = (fst name) ++ " " ++ (snd name)

okinawaOffice name = nameText ++ " - 沖縄県那覇市"
  where nameText = snd name

getLocationFunc location = case location of
  "ho" -> hokkaidoOffice

  "os" -> osakaOffice

  "ok" -> okinawaOffice

  _ -> (\name -> (fst name) ++ " " ++ (snd name))

addressLetter name location = locationFunc name
  where locationFunc = getLocationFunc location
-- 練習問題 4-1
-- Haskellで比較できるものは全てcompare関数で比較できる、これでcompareLastNameを書き換えよう
-- compareLastName name1  name2 = if result == EQ
--                        then compare (fst name1) (fst name2)
--                        else result
-- where result = compare (snd name1) (snd name2)
-- 4-2 
-- 神奈川県横浜市用の住所関数を追加しましょう、名前の後にESQをつけましょう。
-- yokohamaOffice name = nameText ++ "ESQ" ++ " - 神奈川県横浜市"
--   where nameText = (fst name) ++ " " ++ (snd name)
--
-- getLocationFunc location = case location of
--  "yo" -> yokohamaOffice 
