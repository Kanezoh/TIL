import qualified Data.Map as Map

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

-- maybe doで書き直す

names :: Map.Map Int String
names = Map.fromList [(1, "Bob")]

maybeMain :: Maybe String
maybeMain = do
  name <- Map.lookup 1 names
  let statement = helloPerson name
  return statement

-- n番目のフィボナッチ数列を出力
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib(n - 2)

fibMain :: IO()
fibMain = do
  putStrLn "Enter the number"
  index <- getLine
  let ans = fib (read index)
  putStrLn (show ans)
