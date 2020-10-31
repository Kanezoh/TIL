successfulRequest :: Maybe Int
successfulRequest = Just 6

failedRequest :: Maybe Int
failedRequest = Nothing

incMaybe :: Maybe Int -> Maybe Int
incMaybe (Just n) = Just (n + 1)
incMaybe Nothing = Nothing

reverseMaybe :: Maybe String -> Maybe String
-- Functorを使わない場合
--reverseMaybe :: Maybe String -> Maybe String
--reverseMaybe (Just str) = Just (reverse str)
--reverseMaybe Nothing = Nothing
reverseMaybe str = reverse <$> str

-- Functor で型クラスのコンテキストで関数を適用する
-- <$>はfmapのエイリアス 
successStr :: Maybe String
successStr = show <$> successfulRequest

failStr :: Maybe String
failStr = show <$> failedRequest

