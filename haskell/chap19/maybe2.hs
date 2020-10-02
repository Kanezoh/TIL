import qualified Data.Map as Map

data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq, Ord)
data Container = Vat Organ | Cooler Organ | Bag Organ

instance Show Container where
  show (Vat organ)    = show organ ++ " in a vat"
  show (Cooler organ) = show organ ++ " in a cooler"
  show (Bag organ)    = show organ ++ " in a bag"

data Location = Lab | Kitchen | Bathroom deriving Show

organToContainer :: Organ -> Container
organToContainer Brain = Vat Brain
organToContainer Heart = Cooler Heart
organToContainer organ = Bag Brain

placeInLocation :: Container -> (Location, Container)
placeInLocation (Vat a)    = (Lab, Vat a)
placeInLocation (Cooler a) = (Lab, Cooler a)
placeInLocation (Bag a)    = (Kitchen, Bag a)

process :: Organ -> (Location, Container)
process organ = placeInLocation (organToContainer organ)

report :: (Location, Container) -> String
report (location, container) = show container ++
                               " in the " ++
                               show location

-- Maybeに対処する
processRequest :: Int -> Map.Map Int Organ -> String
processRequest id catalog = processAndReport organ
  where organ = Map.lookup id catalog

processAndReport :: Maybe Organ -> String
processAndReport (Just organ) = report (process organ)
processAndReport Nothing = "error, id not found"

maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap f Nothing = Nothing
maybeMap f (Just val) = Just (f val)
 