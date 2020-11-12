data User = User { name    :: String
                 , gamerId :: Int
                 , score   :: Int } deriving Show

testNames :: [String]
testNames = [ "John Smith"
            , "Robert'); DROP TABLE Students; --"
            , "Christina NULL"
            , "Randall Nunroe"]

testIds :: [Int]
testIds = [ 1337
          , 0123
          , 999999 ]

testScores :: [Int]
testScores = [ 0
             , 100000
             , -999999]

testData :: [User]
testData = pure User <*> testNames <*> testIds <*> testScores 
