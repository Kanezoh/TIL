-- 型に別名を付ける型シノニム
type FirstName  = String
type MiddleName = String 
type LastName   = String

name :: FirstName
name = "John"

-- 新しい型を作成する
data Sex = Male | Female
sexInitial :: Sex -> Char
sexInitial Male = 'M'
sexInitial Female = 'F'

-- data Name = Name FirstName MiddleName LastName
data Name = Name { firstName :: FirstName
                 , middleName :: MiddleName
                 , lastName :: LastName
}
name2 = Name "John" "Author" "Hey"

name3 = Name { firstName = "firstName"
          , middleName = "middleName"
          , lastName = "lastName"
}

showName (Name f m l) = (show f) ++ (show m) ++ (show l)
