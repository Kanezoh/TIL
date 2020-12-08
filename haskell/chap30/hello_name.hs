askForName :: IO()
askForName = putStrLn "What is your name?"

nameStatement :: String -> String
nameStatement name = "Hello, " ++ name ++ "!"

helloName :: IO()
helloName = askForName >> getLine >>= (\name -> return (nameStatement name)) >>= putStrLn

-- 練習問題1
allFmapM :: Monad m => (a -> b) -> m a -> m b
allFmapM func val = val >>= (\x -> return (func x))
