-- cup = \flOz -> \message -> message flOz
cup flOz = \message -> message flOz

-- アクセサ、カップに入っているコーヒーの量を示す
getOz aCup = aCup (\flOz -> flOz)


-- カップのコーヒーを飲む＝飲んだ量だけ減ったカップを返す
drink aCup ozDrank = if ozDiff >= 0
                     then cup ozDiff
                     else cup 0
  where flOz = getOz aCup
        ozDiff = flOz - ozDrank

-- カップが空かどうか
isEmpty aCup = getOz aCup == 0
