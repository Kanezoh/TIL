module Main where
  -- 型シノニム、型に別名を定義できる
  type PatientName = (String, String)
  type FirstName = String
  type LastName = String
  type MiddleName = String
  type Age = Int
  type Height = Int

  firstName :: PatientName -> String
  firstName patient = fst patient
  lastName :: PatientName -> String
  lastName patient = snd patient

  -- 独自の型
  data Name =  Name FirstName LastName | NameWithMiddle FirstName MiddleName LastName
  data Sex = Male | Female
  sexInitial :: Sex -> Char
  sexInitial Male = 'M'
  sexInitial Female = 'F'

  showSex Male = "Male"
  showSex Female = "Female"

  data RhType = Pos | Neg
  data ABOType = A | B | AB | O
  data BloodType = BloodType ABOType RhType

  -- 普通の型定義
  -- data Patient = Patient Name Sex Int Int Int BloodType
  -- レコード構文を使用
  data Patient = Patient { name:: Name
                         , sex :: Sex
                         , age :: Int
                         , height :: Int
                         , weight :: Int
                         , bloodType :: BloodType }
  jackSmith :: Patient
  jackSmith = Patient { name = Name "Jack" "Smith"
                      , age = 43
                      , sex = Male
                      , height = 62
                      , weight = 115
                      , bloodType = BloodType O Neg }

  
  showName :: Name -> String
  showName (Name f l) = f ++ " " ++ l
  showName (NameWithMiddle f m l) = f ++ " " ++ m ++ " " ++ l

  showRh :: RhType -> String
  showRh Pos = "+"
  showRh Neg = "-"

  showABO :: ABOType -> String
  showABO A = "A"
  showABO B = "B"
  showABO AB = "AB"
  showABO O = "O"

  showBloodType :: BloodType -> String
  showBloodType (BloodType abo rh) = showABO abo ++ showRh rh

  -- 輸血できるかどうかを判定
  canDonateTo :: BloodType -> BloodType -> Bool
  canDonateTo (BloodType O _) _ = True
  canDonateTo _ (BloodType AB _) = True
  canDonateTo (BloodType A _) (BloodType A _) = True
  canDonateTo (BloodType B _) (BloodType B _) = True
  canDonateTo _ _ = False

  -- 患者バージョン
  canDonateToPatient :: Patient -> Patient -> Bool
  canDonateToPatient p1 p2 = canDonateTo (bloodType p1) (bloodType p2)

  patientSummary :: Patient -> String
  patientSummary p = "****************************\n" ++
                     "Patient Name: " ++ showName (name p) ++ ", " ++ "\n" ++
                     "Sex: " ++ showSex (sex p) ++ "\n" ++
                     "Age: " ++ show (age p) ++ "\n" ++
                     "Height: " ++ show (height p) ++ "\n" ++
                     "Weight: " ++ show (weight p) ++ "\n" ++
                     "Blood Type: " ++ showBloodType (bloodType p)

  -- 普通のpatientの型シグネチャ
  -- patientInfo :: String -> String -> Int -> Int -> String

  -- カスタム型を使用したシグネチャ
  patientInfo :: PatientName -> Age -> Height -> String

  patientInfo patientName age height = name ++ " " ++ ageHeight
    where name = (firstName patientName) ++ ", " ++ (lastName patientName)
          ageHeight = "(" ++ show age ++ "yrs." ++ show height ++ "in.)"
  main = print "ababa"
