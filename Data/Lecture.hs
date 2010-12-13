module Data.Lecture where

import Data.Room
import Data.Day
import Data.Time
import Data.Students
import Data.Lecturer

{- |
  'Lecture' represents all possibilities for one lecture befor planing
-}

data Lecture = Lecture { name :: String
                       , rooms :: [Room]
                       , timeslots :: [(Day,[Time])]
                       , students :: [Students]
                       , lecturer :: [Lecturer]
                       , equipment :: [String]
                       }
  deriving Eq

instance Show Lecture where
  show (Lecture name rooms timeslots students lecturer equipment) =
    "Lecture " ++ name
    ++ "\n\tRooms:     " ++ (show rooms)
    ++ "\n\tTimeslots: " ++ (show timeslots)
    ++ "\n\tStudents:  " ++ (show students)
    ++ "\n\tLecturer:  " ++ (show lecturer)
    ++ "\n\tEquipment: " ++ (show equipment) ++ "\n"

