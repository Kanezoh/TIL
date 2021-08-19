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

-- ダメージを与える
damage aRobot attackDamage = aRobot (\(n,a,h) -> robot (n, a, h-attackDamage))

fight aRobot defender = damage defender attack
  where attack = if getHp aRobot > 10
                 then getAttack aRobot
                 else 0

gentleGiant = robot ("Mr Friendly", 10 , 300)

gentleGiantRound1 = fight killerRobot gentleGiant
killerRobotRound1 = fight gentleGiant killerRobot
gentleGiantRound2 = fight killerRobotRound1 gentleGiantRound1
killerRobotRound2 = fight gentleGiantRound1 killerRobotRound1
gentleGiantRound3 = fight killerRobotRound2 gentleGiantRound2
killerRobotRound3 = fight gentleGiantRound2 killerRobotRound2
