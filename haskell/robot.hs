-- コンストラクタ
robot (name,attack,hp) = \message -> message (name,attack,hp)

killerRobot = robot ("Kill3r",25,200)

name (n,_,_)  = n
attack(_,a,_) = a
hp (_,_,h)    = h

-- ゲッター
getName aRobot   = aRobot name
getAttack aRobot = aRobot attack
getHp aRobot     = aRobot hp

-- セッター
setName aRobot newName     = aRobot (\(n,a,h) -> robot (newName, a, h))
setAttack aRobot newAttack = aRobot (\(n,a,h) -> robot (n, newAttack, h))
setHp aRobot newHp         = aRobot (\(n,a,h) -> robot (n, a, newHp))

-- Robotの情報を出力する
printRobot aRobot = aRobot (\(n,a,h) -> n ++ 
                                        " attack:" ++ (show a) ++
                                        " hp:" ++ (show h))
