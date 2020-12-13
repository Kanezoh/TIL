import Control.Monad

data Name = Name { firstName :: String, lastName :: String}

instance Show Name where
  show (Name first last) = mconcat [first, " ", last]

data GradeLevel = Freshman | Sophomore | Junior | Senior deriving (Eq,Ord,Enum,Show)

data Student = Student { studentId :: Int
                       , gradeLevel :: GradeLevel
                       , studentName :: Name } deriving Show

students :: [Student]
students = [ (Student 1 Senior (Name "Audre" "Lorde"))
           , (Student 2 Junior (Name "Lesile" "Silko"))
           , (Student 3 Freshman (Name "Judith" "Butler"))
           , (Student 4 Senior (Name "Guy" "Debord"))
           , (Student 5 Sophomore (Name "Jean" "Baudrillard"))
           , (Student 6 Junior (Name "Julia" "Kristeva")) ]

-- select関数
_select :: (a -> b) -> [a] -> [b]
_select prop vals = do
  val <- vals
  return (prop val)

-- where関数
_where :: (a -> Bool) -> [a] -> [a]
_where tests vals = do
  val <- vals
  guard (tests val)
  return val

startsWith :: Char -> String -> Bool
startsWith char string = char == head string
