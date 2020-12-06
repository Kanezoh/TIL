echo :: IO()
echo = getLine >>= putStrLn
-- getLine :: IO String
-- putStrLn :: String -> IO()

main :: IO()
main = echo
