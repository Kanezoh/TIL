import qualified Data.Map as Map

data RobotPart = RobotPart { name        :: String
                           , description :: String
                           , cost        :: Double
                           , count       :: Int } deriving Show

leftArm :: RobotPart
leftArm = RobotPart { name = "left arm"
                    , description = "left arm for face punching"
                    , cost = 1000.00
                    , count = 3
}

rightArm :: RobotPart
rightArm = RobotPart { name = "rightt arm"
                    , description = "right arm for kind hand gestures"
                    , cost = 1025.00
                    , count = 5
}

robotHead :: RobotPart
robotHead = RobotPart { name = "robot head"
                    , description = "this head looks mad"
                    , cost = 5092.25
                    , count = 2
}

type Html = String
renderHtml :: RobotPart -> Html
renderHtml part = mconcat ["<h2>", partName, "</h2>"
                         , "<p><h3>desc</h3>", partDesc
                         , "</p><p><h3>cost</h3>"
                         , partCost
                         , "</p><p><h3>count</h3>"
                         , partCount, "</p>"]
  where partName = name part
        partDesc = description part
        partCost = show (cost part)
        partCount = show (count part)

partsDB :: Map.Map Int RobotPart
partsDB = Map.fromList keyVals
  where keys = [1, 2, 3]
        vals = [leftArm, rightArm, robotHead]
        keyVals = zip keys vals

--insertSnippet :: Maybe Html -> IO ()

partVal :: Maybe RobotPart
partVal = Map.lookup 1 partsDB

partHtml :: Maybe Html
partHtml = renderHtml <$> partVal

allParts :: [RobotPart]
allParts = snd <$> Map.toList partsDB

allPartsHtml :: [Html]
allPartsHtml = renderHtml <$> allParts

-- partsDBをHTMLに変換する
htmlPartsDB :: Map.Map Int Html
htmlPartsDB = renderHtml <$> partsDB

-- IOから得られるleftArmIO
leftArmIO :: IO RobotPart
leftArmIO = return leftArm

htmlSnippet :: IO Html
htmlSnippet = renderHtml <$> leftArmIO

-- 練習問題 27-1
newtype Box a = Box a deriving Show
instance Functor Box where
  fmap func (Box val) = Box (func val)

morePresents :: Int -> Box a -> Box [a]
morePresents count present = fmap (nCopies count) present
  where nCopies count item = (take count . repeat) item

-- 練習問題 27-2
myBox :: Box Int
myBox = Box 1

wrap :: a -> Box a
wrap val = Box val

unwrap :: Box a -> a
unwrap (Box val) = val

-- 練習問題 27-3
printCost :: Maybe Double -> IO()
printCost Nothing = putStrLn "item not found"
printCost (Just cost) = print cost

main :: IO()
main = do
  putStrLn "enter a part number"
  partNo <- getLine
  let part = Map.lookup (read partNo) partsDB
  printCost (cost <$> part)
