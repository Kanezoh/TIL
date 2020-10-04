helloPerson :: String -> String
helloPerson name = "Hello" ++ " " ++ name ++ "!"

-- mainをIO型として宣言、()は空のタプルを表す
-- これはmain関数が何を返すか分からないためである
main :: IO()
-- do記法、型IOを型aのように扱える
-- <- を使用する変数ではIO型をa型のように扱える
main = do
  putStrLn "Hello, What's your name?"
  -- getLineはI/Oアクション、返り値はIO String
  name <- getLine
  -- helloPersonは String -> Stringだがdo表記のおかげでうまくいく
  let statement = helloPerson name
  putStrLn statement

-- I/Oアクションの例

-- Stringを引数に取ってIOを返す
-- putStrLn :: String -> IO ()

-- 引数なしでIOを返す
-- 全ての関数が引数を受け取らなければならないという原則に反している
-- getLine :: IO String
