import Data.List.Split

calc :: [String] -> Int
calc (n1:"+":n2:rest) = (read n1 :: Int) + (read n2 :: Int)
calc (n1:"*":n2:rest) = (read n1 :: Int) * (read n2 :: Int)

main :: IO()
main = do
  userInput <- getContents
  let elems = lines userInput
  print (calc elems)
