import Control.Monad

data Name = Name { firstName :: String, lastName :: String}

instance Show Name where
  show (Name first last) = mconcat [first, " ", last]

data GradeLevel = Freshman | Sophomore | Junior | Senior deriving (Eq,Ord,Enum,Show)

data Student = Student { studentId :: Int
                       , gradeLevel :: GradeLevel
                       , studentName :: Name } deriving Show

data Teacher = Teacher { teacherId :: Int, teacherName :: Name } deriving Show
teachers :: [Teacher]
teachers = [ Teacher 100 (Name "Simone" "De Beauvior")
          , Teacher 200 (Name "Susan" "Sontag")]

data Course = Course { courseId :: Int
                     , courseTitle :: String
                     , teacher :: Int } deriving Show
courses :: [Course]
courses = [Course 101 "French" 100, Course 201 "English" 200]

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

_join :: Eq c => [a] -> [b] -> (a -> c) -> (b -> c) -> [(a,b)]
_join data1 data2 prop1 prop2 = do
  d1 <- data1
  d2 <- data2
  let dpairs = (d1,d2)
  return dpairs
  guard ((prop1 (fst dpairs)) == (prop2 (snd dpairs)))
  return dpairs
