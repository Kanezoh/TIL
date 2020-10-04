import System.Random

minDie :: Int
minDie = 1

maxDie :: Int
maxDie = 6

main :: IO ()
main = do
  -- 実行するたびに結果が違う=参照透過性に反するのでrandomRIOはI/Oアクション
  dieRoll <- randomRIO (minDie, maxDie)
  putStrLn (show dieRoll)
