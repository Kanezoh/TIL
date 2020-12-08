askForName :: IO()
askForName = putStrLn "What is your name?"

nameStatement :: String -> String
nameStatement name = "Hello, " ++ name ++ "!"

helloName :: IO()
helloName = askForName >> getLine >>= (\name -> return (nameStatement name)) >>= putStrLn

-- 練習問題1
allFmapM :: Monad m => (a -> b) -> m a -> m b
allFmapM func val = val >>= (\x -> return (func x))

-- 練習問題2
allApp :: Monad m => m(a -> b) -> m a -> m b
allApp func val = func >>= (\f -> val >>= (\x -> return (f x)))

-- 練習問題3
bind :: Maybe a -> (a -> Maybe b) -> Maybe b
bind Nothing _ = Nothing
bind (Just val) func = func val 
