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

  main = do print "aaa"
