echo :: IO()
echo = getLine >>= putStrLn
-- getLine :: IO String
-- putStrLn :: String -> IO()

main :: IO()
main = echo

-- クイックチェック3
readInt :: IO Int
readInt = read <$> getLine

printDouble :: Int -> IO()
printDouble n = print (n*2)

echoDouble :: IO()
echoDouble = readInt >>= printDouble
