quotes :: [String]
quotes = ["First","second","third","fourth","fifth"]

lookupQuote :: [String] -> [String]
lookupQuote [] = []
lookupQuote ("n":xs) = []
lookupQuote (x:xs) = quote : lookupQuote xs
  where quote = quotes !! (read x - 1)

main :: IO()
main = do
  userInput <- getContents
  mapM_ putStrLn (lookupQuote (lines userInput))
