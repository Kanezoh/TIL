module Main where
  
  -- コンストラクタ
  robot (name, attack, hp) = \message -> message(name, attack, hp)
  -- 使い方
  -- killerRobot = robot ("Kill3r", 25, 200)
  -- ヘルパー関数
  name (n, _, _) = n
  attack (_, a, _) = a
  hp (_, _, hp) = hp
  -- アクセサ
  getName aRobot = aRobot name
  getAttack aRobot = aRobot attack
  getHp aRobot = aRobot hp
  -- セッター
  setName aRobot newName = aRobot (\(n, a, h) -> robot (newName, a, h))
  setAttack aRobot newAttack = aRobot (\(n, a, h) -> robot (n, newAttack, h))
  setHp aRobot newHp = aRobot (\(n, a, h) -> robot (n, a, newHp))
  -- robotのステータスを表示
  printRobot aRobot = aRobot (\(n, a, h) -> n ++
                                            " attack:" ++ (show a) ++
                                            " hp:" ++ (show h))

  main = do print "aaa"
