module Main where
  -- 型シノニム、型に別名を定義できる
  type PatientName = (String, String)
  type Age = Int
  type Height = Int

  firstName :: PatientName -> String
  firstName patient = fst patient
  lastName :: PatientName -> String
  lastName patient = snd patient

  -- 普通のpatientの型シグネチャ
  -- patientInfo :: String -> String -> Int -> Int -> String

  -- カスタム型を使用したシグネチャ
  patientInfo :: PatientName -> Age -> Height -> String

  patientInfo patientName age height = name ++ " " ++ ageHeight
    where name = (firstName patientName) ++ ", " ++ (lastName patientName)
          ageHeight = "(" ++ show age ++ "yrs." ++ show height ++ "in.)"
  main = print "ababa"
