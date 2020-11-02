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
