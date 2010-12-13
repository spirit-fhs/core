module Data.OneLecture where

import Data.Room
import Data.Day
import Data.Time
import Data.Students
import Data.Lecturer

{- |
  'OneLecture' represents one lecture in and after the planingprozess
-}

data OneLecture = OneLecture { name :: String
                             , room :: Room
                             , day :: Day
                             , time :: Time
                             , students :: [Students]
                             , lecturer :: [Lecturer]
                             }

  deriving Eq

instance Ord OneLecture where
  x <= y  =
    compare (sizeOneLecture x) (sizeOneLecture y) /= GT
  x < y =
    compare (sizeOneLecture x) (sizeOneLecture y) == LT
  x >= y =
    compare (sizeOneLecture x) (sizeOneLecture y) /= LT
  x > y  =
    compare (sizeOneLecture x) (sizeOneLecture y) == GT

{- |
  'sizeOneLecture' calculates the size of the lecture
-}

sizeOneLecture :: OneLecture -> Students
sizeOneLecture (OneLecture _ _ _ _ students _) =
  foldl (+)(Students [] 0) students

instance Show OneLecture where
  show (OneLecture name room day time students lecturer) =
    "Lecture " ++ name
    ++ "\n\tRoom:     " ++ (show room)
    ++ "\n\tDay:      " ++ (show day)
    ++ "\n\tTime:     " ++ (show time)
    ++ "\n\tStudents: " ++ (show students)
    ++ "\n\tLecturer: " ++ (show lecturer)
    ++ "\n"

