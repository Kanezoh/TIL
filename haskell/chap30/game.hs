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

creditsFromId :: GamerId -> Maybe PlayerCredits

lookupUserName :: GamerId -> Maybe UserName
lookupUserName id = Map.lookup id userNameDB

lookupCredits :: UserName -> Maybe PlayerCredits
lookupCredits username = Map.lookup username creditsDB
