--クイックチェック5-1 
--genIfXEven関数を作成しろ、xを使ってクロージャを作成して新しい関数を返す
--返された関数はxが偶数の場合にxに適用する関数として渡すことができる
--ifEven f x = if even x
--             then f x
--             else x
--genIfXEven f = (\x -> ifEven f x)
--
--クイックチェック5-2
--genApiRequestBuilderを書き換え、引数としてリソースも受け取るバージョンを作成してみよう
--genApiRequestBuilder hostBuilder apikey resource = (\id ->
--                                            hostBuilder apikey resource id)
-- クイックチェック5-3
-- URLがhttp://example.com APIキーが1337hAsk311,リソースがbookのビルダー関数を作れ
--basicBuilder host apikey resource id = host ++ "/" ++ resource ++ "/" ++
--                                       id ++ "?token=" ++ apikey
--resourceBuilder = basicBuilder "http://example.com" "1337hAsk311" "book" 

-- クイックチェック5-4
-- flipと部分適用を使ってsubtract2という関数を作成しろ、この関数は渡された数値から2を引くものとする
subtract2 = flip (-) 2

--練習問題5-1 ifEven関連の関数を部分適用で作れ
ifEvenInc = ifEven inc
ifEvenDouble = ifEven double
ifEvenSquare = ifEven square

--5-2 binaryPartialApplicationを作れ、二項関数と引数を受け取って不明な引数を期待する
binaryPartialApplication func arg = (\x -> func arg x)

