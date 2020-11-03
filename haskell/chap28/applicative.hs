import qualified Data.Map as Map

type LatLog = (Double, Double)

locationDB :: Map.Map String LatLog
locationDB = Map.fromList [("Arkham", (42.6054, -70.7829))
                          ,("Innsmouth", (42.8250, -70.8150))
                          ,("Carcosa", (29.9714, -90.7694))
                          ,("New York", (40.7776, -73.9691))]

toRadians :: Double -> Double
toRadians degrees = degrees * pi / 180

latLongToRads :: LatLog -> (Double, Double)
latLongToRads (lat, log) = (rlat, rlog)
  where rlat = toRadians lat
        rlog = toRadians log

haversine :: LatLog -> LatLog -> Double
haversine coords1 coords2 = earthRadius * c
  where (rlat1, rlog1) = latLongToRads coords1
        (rlat2, rlog2) = latLongToRads coords2
        dlat = rlat2 - rlat1
        dlog = rlog2 - rlog1
        a = sin (dlat / 2)^ 2 + cos rlat1 * cos rlat2 * sin (dlog / 2)^ 2
        c = 2 * atan2 (sqrt a) (sqrt (1 - a))
        earthRadius = 3961.0

printDistance :: Maybe Double -> IO()
printDistance Nothing = putStrLn "Error, invalid city entered"
printDistance (Just distance) = putStrLn (show distance ++ "miles")

-- クイックチェック1
addMaybe :: Maybe Int -> Maybe Int -> Maybe Int
addMaybe (Just a) (Just b) = Just (a + b)
addMaybe _ _ = Nothing 

-- Functorに部分適用を試す
maybeInc = (+) <$> Just 1
-- 型シグネチャがMaybe (Integer -> Integer)になっている
-- これを適用するにはApplicative型クラスの<*> 演算子が使える
-- ex) maybeInc <*> Just 5
-- (++) <$> Just "cats" <*> Just "and dogs"
val1 = Just 10
val2 = Just 5
--(*) <$> val1 <*> val2
--(div) <$> val1 <*> val2
--(mod) <$> val1 <*> val2

main :: IO ()
main = do
  putStrLn "Enter the starting city name:"
  startingInput <- getLine
  let startingCity = Map.lookup startingInput locationDB
  putStrLn "Enter the destination city name:"
  destInput <- getLine
  let destCity = Map.lookup destInput locationDB
  let distance = haversine <$> startingCity <*> destCity
  printDistance distance

-- 練習問題1
haversineIO :: IO LatLog -> IO LatLog -> IO Double
haversineIO ioVal1 ioVal2 = do
  val1 <- ioVal1
  val2 <- ioVal2
  let dist = haversine val1 val2
  return dist
-- 練習問題2
-- haversineIO <$> ioVal1 <*> ioVal2
