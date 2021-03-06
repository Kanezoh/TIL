import qualified Data.Map as Map

askForName :: IO()
askForName = putStrLn "What is your name?"

nameStatement :: String -> String
nameStatement name = "Hello, " ++ name ++ "!"

helloNameDo :: IO()
helloNameDo = do
  askForName
  name <- getLine
  putStrLn (nameStatement name)

echo :: IO()
echo = do
  str <- getLine
  putStrLn str

data Grade  = F | D | C | B | A deriving (Eq, Ord, Enum, Show, Read)
data Degree = HS | BA | MS | PhD deriving (Eq, Ord, Enum, Show, Read) 

data Candidate = Candidate { candidateId :: Int
                           , codeReview  :: Grade
                           , cultureFit  :: Grade
                           , education   :: Degree } deriving Show

viable :: Candidate -> Bool
viable candidate = all (== True) tests
  where passedCoding     = codeReview candidate > B
        passedCultureFit = cultureFit candidate > C
        educationMin     = education candidate >= MS
        tests = [passedCoding, passedCultureFit, educationMin]

readInt :: IO Int
readInt = getLine >>= (return . read)

readGrade :: IO Grade
readGrade = getLine >>= (return . read)

readDegree :: IO Degree
readDegree = getLine >>= (return .read)

readCandidate :: IO Candidate
readCandidate = do
  putStrLn "enter id:"
  cId <- readInt
  putStrLn "enter code grade:"
  codeGrade <- readGrade
  putStrLn "enter culture fit grade"
  cultureGrade <- readGrade
  putStrLn "enter education"
  degree <- readDegree
  return (Candidate { candidateId = cId
                    , codeReview  = codeGrade
                    , cultureFit  = cultureGrade
                    , education   = degree })

assessCandidateIO :: IO String
assessCandidateIO = do
  candidate <- readCandidate
  let passed = viable candidate
  let statement = if passed
                  then "passed"
                  else "failed"
  return statement
-- 候補者のデータ
candidate1 :: Candidate
candidate1 = Candidate { candidateId = 1
                       , codeReview  = A
                       , cultureFit  = A
                       , education   = BA }

candidate2 :: Candidate
candidate2 = Candidate { candidateId = 2
                       , codeReview  = C
                       , cultureFit  = A
                       , education   = PhD }


candidate3 :: Candidate
candidate3 = Candidate { candidateId = 3
                       , codeReview  = A
                       , cultureFit  = B
                       , education   = MS }

candidateDB :: Map.Map Int Candidate
candidateDB = Map.fromList [(1,candidate1), (2,candidate2), (3,candidate3)]

-- Maybe型のassess関数、MapのlookupではMaybeが返ってくるため
assessCandidateDataMaybe :: Int -> Maybe String
assessCandidateDataMaybe cId = do
  candidate <- Map.lookup cId candidateDB
  let passed = viable candidate
  let statement = if passed
                  then "passed"
                  else "failed"
  return statement

candidates :: [Candidate]
candidates = [candidate1,candidate2,candidate3]

assessCandidateList :: [Candidate] -> [String]
assessCandidateList candidates = do
  candidate <- candidates
  let passed = viable candidate
  let statement = if passed
                  then "passed"
                  else "failed"
  return statement

assessCandidate :: Monad m => m Candidate -> m String
assessCandidate candidates = do
  candidates <- candidates
  let passed = viable candidates
  let statement = if passed
                  then "passed"
                  else "failed"
  return statement

-- 練習問題2
listMain :: [String]
listMain = do
  size1 <- [1,2,3]
  cost1 <- [100,200,300]
  size2 <- [4,5,6]
  cost2 <- [400,500,600]
  let pizza1 = (size1,cost1)
  let pizza2 = (size2,cost2)
  let betterPizza = comparePizzas pizza1 pizza2
  return (describePizza betterPizza)

-- 練習問題3
monadMain :: Monad m => m Double -> m Double -> m Double -> m Double -> m String
monadMain s1 c1 s2 c2 = do
  size1 <- s1
  cost1 <- c1
  size2 <- s2
  cost2 <- c2
  let pizza1 = (size1,cost1)
  let pizza2 = (size2,cost2)
  let betterPizza = comparePizzas pizza1 pizza2
  return (describePizza betterPizza)
