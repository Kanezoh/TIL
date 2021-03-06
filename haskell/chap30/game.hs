import qualified Data.Map as Map

type UserName = String
type GamerId  = Int
type PlayerCredits = Int

-- GamerIdに基づいてUserNameを取得するためのデータベース
userNameDB :: Map.Map GamerId UserName
userNameDB = Map.fromList [ (1, "nYarlathoTep")
                          , (2, "KinGinYELLOW")
                          , (3, "dagon1997")
                          , (4, "rcarter1919")
                          , (5, "xCTHULHUx")
                          , (6, "yogSOThoth") ]

-- userNameに基づいてPlayerCreditsを調べるためのデータベース
creditsDB :: Map.Map UserName PlayerCredits
creditsDB = Map.fromList [ ("nYarlathoTep", 2000)
                         , ("KinGinYELLOW", 15000)
                         , ("dagon1997", 300)
                         , ("rcarter1919", 12)
                         , ("xCTHULHUx", 50000)
                         , ("yogSOThoth", 150000)]

lookupUserName :: GamerId -> Maybe UserName
lookupUserName id = Map.lookup id userNameDB

lookupCredits :: UserName -> Maybe PlayerCredits
lookupCredits username = Map.lookup username creditsDB

altLookupCredits :: Maybe UserName -> Maybe PlayerCredits
altLookupCredits Nothing = Nothing
altLookupCredits (Just username) = lookupCredits username

creditsFromId :: GamerId -> Maybe PlayerCredits
--creditsFromId id = altLookupCredits (lookupUserName id)

-- Monadのbind演算子(>>=)を使ったcreditsFromIdの実装
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
creditsFromId id = lookupUserName id >>= lookupCredits

type WillCoId = Int

gamerIdDB :: Map.Map WillCoId GamerId
gamerIdDB = Map.fromList [(1001,1), (1002,2), (1003,3), (1004,4), (1005,5), (1006,6)]

lookupGamerId :: WillCoId -> Maybe GamerId
lookupGamerId id = Map.lookup id gamerIdDB

creditsFromWCId :: WillCoId -> Maybe PlayerCredits
creditsFromWCId id = lookupGamerId id >>= lookupUserName >>= lookupCredits
