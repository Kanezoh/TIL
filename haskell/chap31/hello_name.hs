askForName :: IO()
askForName = putStrLn "What is your name?"

nameStatement :: String -> String
nameStatement name = "Hello, " ++ name ++ "!"

helloNameDo :: IO()
helloNameDo = do
  askForName
  name <- getLine
  putStrLn (nameStatement name)

echo :: IO()
echo = do
  str <- getLine
  putStrLn str
