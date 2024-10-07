import Control.Monad
import Control.Applicative

data Name = Name
            { firstName :: String
            , lastName :: String }

instance Show Name where
    show (Name first last) = mconcat [first, " ", last]

data GradeLevel = Freshman
                | Sophmore
                | Junior
                | Senior deriving (Eq,Ord,Show,Enum)

data Student = Student
               { studentId :: Int
               , gradeLevel :: GradeLevel
               , studentName :: Name } deriving Show

data Teacher = Teacher
               { teacherId :: Int
               , teacherName :: Name } deriving Show

data Enrollment = Enrollment
                  { student :: Int
                  , course :: Int } deriving Show

data Course = Course
              { courseId :: Int
              , courseTitle :: String
              , teacher :: Int } deriving Show

data HINQ m a b = HINQ (m a -> m b) (m a) (m a -> m a)
                | HINQ_ (m a -> m b) (m a)

-- Select and Where functions
--
-- select :: (a -> b) -> [a] -> [b]
--
-- test function for where :: (a -> Bool) -> [a] -> [a]
--
-- join function :: Eq c => [a] -> [b] -> (a -> c) -> (b -> c) -> [(a,b)]
--
-- functions preceded by underscore to avoid name conflicts

-- _select :: (a -> b) -> [a] -> [b]
_select :: Monad m => (a -> b) -> m a -> m b
_select prop vals = do
    val <- vals
    return (prop val)

-- ghci> _select (firstName . studentName) students
-- ["Audre","Leslie","Judith","Guy","Jean","Julia"]
-- ghci> _select gradeLevel students
-- [Senior,Junior,Freshman,Senior,Sophmore,Junior]
-- ghci> _select (\x -> (studentName x, gradeLevel x)) students
-- [(Audre Lorde,Senior),(Leslie Silko,Junior),
-- (Judith Butler,Freshman),(Guy Debord,Senior),
-- (Jean Baudrillard,Sophmore),(Julia Kristeva,Junior)]

-- _where :: (a -> Bool) -> [a] -> [a]
_where :: (Monad m, Alternative m) => (a -> Bool) -> m a -> m a
_where test vals = do
    val <- vals
    guard (test val)
    return val

startsWith :: Char -> String -> Bool
startsWith char string = char == (head string)

-- ghci> _where (startsWith 'J' . firstName) (_select studentName students)
-- [Judith Butler,Jean Baudrillard,Julia Kristeva

-- select * from
-- teachers inner join courses
-- on (teachers.teacherId = courses.teacher)

-- _join :: Eq c => [a] -> [b] -> (a -> c) -> (b -> c) -> [(a,b)]
_join :: (Monad m, Alternative m, Eq c) => m a -> m b -> (a -> c) -> (b -> c) -> m (a,b)
_join data1 data2 prop1 prop2 = do
    d1 <- data1
    d2 <- data2
    let dpairs = (d1,d2)
    guard ((prop1 (fst dpairs)) == (prop2 (snd dpairs)))
    return dpairs

-- ghci> _join teachers courses teacherId teacher
-- [(Teacher {teacherId = 100, teacherName = Simone De Beauvior},Course {courseId = 101, courseTitle = "French", teacher = 100}),(Teacher {teacherId = 200, teacherName = Susan Sontag},Course {courseId = 201, courseTitle = "English", teacher = 200})]

joinData = (_join teachers courses teacherId teacher)
whereResult = _where ((== "English") . courseTitle . snd) joinData
selectResult = _select (teacherName . fst) whereResult

_hinq selectQuery joinQuery whereQuery = (\joinData ->
                                          (\whereResult ->
                                           selectQuery whereResult)
                                          (whereQuery joinData)
                                         ) joinQuery

finalResult :: [Name]
finalResult = _hinq (_select (teacherName . fst))
                    (_join teachers courses teacherId teacher)
                    (_where ((== "English") . courseTitle . snd))

-- If an argument is missing:
teacherFirstName :: [String]
teacherFirstName = _hinq (_select firstName)
                         finalResult
                         (_where (\_ -> True))

runHINQ :: (Monad m, Alternative m) => HINQ m a b -> m b
runHINQ (HINQ sClause jClause wClause) = _hinq sClause jClause wClause
runHINQ (HINQ_ sClause jClause) = _hinq sClause jClause (_where (\_ -> True))

query1 :: HINQ [] (Teacher, Course) Name
query1 = HINQ (_select (teacherName . fst))
              (_join teachers courses teacherId teacher)
              (_where ((== "English") . courseTitle . snd))

-- ghci> runHINQ query1
-- [Susan Sontag]

query2 :: HINQ [] Teacher Name
query2 = HINQ_ (_select teacherName)
               teachers

-- ghci> runHINQ query2
-- [Simone De Beauvior,Susan Sontag]

possibleTeacher :: Maybe Teacher
possibleTeacher = Just (head teachers)

possibleCourse :: Maybe Course
possibleCourse = Just (head courses)

maybeQuery1 :: HINQ Maybe (Teacher, Course) Name
maybeQuery1 = HINQ (_select (teacherName . fst))
                   (_join possibleTeacher possibleCourse teacherId teacher)
                   (_where ((== "French") . courseTitle . snd))

-- ghci> runHINQ maybeQuery1
-- Just Simone De Beauvior

missingCourse :: Maybe Course
missingCourse = Nothing

maybeQuery2 :: HINQ Maybe (Teacher, Course) Name
maybeQuery2 = HINQ (_select (teacherName . fst))
                   (_join possibleTeacher missingCourse teacherId teacher)
                   (_where ((== "French") . courseTitle . snd))

-- ghci> runHINQ maybeQuery2
-- Nothing

-- studentEnrollmentsQ = HINQ_ (_select (\(st,en) ->
   --                                    (studentName st, course en))
     --                       (_join students enrollments studentId student)

studentEnrollmentsQ :: HINQ [] (Student, Enrollment) (Name, Int)
studentEnrollmentsQ = HINQ_ (_select (\(st,en) -> (studentName st, course en)))
                            (_join students enrollments studentId student)

studentEnrollments :: [(Name, Int)]
studentEnrollments = runHINQ studentEnrollmentsQ

englishStudentsQ = HINQ (_select (fst . fst))
                        (_join studentEnrollments
                               courses
                               snd
                               courseId)
                        (_where ((== "English") . courseTitle . snd))

englishStudents :: [Name]
englishStudents = runHINQ englishStudentsQ

getEnrollments :: String -> [Name]
getEnrollments courseName = runHINQ courseQuery
    where courseQuery = HINQ (_select (fst . fst))
                             (_join studentEnrollments
                                    courses
                                    snd
                                    courseId)
                             (_where ((== courseName) . courseTitle . snd))

students :: [Student]
students = [(Student 1 Senior (Name "Audre" "Lorde"))
           ,(Student 2 Junior (Name "Leslie" "Silko"))
           ,(Student 3 Freshman (Name "Judith" "Butler"))
           ,(Student 4 Senior (Name "Guy" "Debord"))
           ,(Student 5 Sophmore (Name "Jean" "Baudrillard"))
           ,(Student 6 Junior (Name "Julia" "Kristeva"))]

teachers :: [Teacher]
teachers = [Teacher 100 (Name "Simone" "De Beauvior")
           ,Teacher 200 (Name "Susan" "Sontag")]

courses :: [Course]
courses = [Course 101 "French" 100
          ,Course 201 "English" 200]

enrollments :: [Enrollment]
enrollments = [(Enrollment 1 101)
              ,(Enrollment 2 101)
              ,(Enrollment 2 201)
              ,(Enrollment 3 101)
              ,(Enrollment 4 201)
              ,(Enrollment 4 101)
              ,(Enrollment 5 101)
              ,(Enrollment 6 201) ]
