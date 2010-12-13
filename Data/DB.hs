module Data.DB where

import Data.Room
import Data.Day
import Data.Time
import Data.Semester
import Data.Students
import Data.Lecturer
import Data.Lecture
import Data.Timetable

{-
  The module Data.DB represents the knowledge base of the timetable
-}


{- |
  'allRooms' represents a list of all available rooms at the FHS
-}

allRooms :: [Room]
allRooms = [ Room "F" "111" 110 ["Beamer"]
           , Room "F" "PC3" 50 ["PC"]
           , Room "F" "203" 80 ["Beamer"]
           ]

{- |
  'initialLecture' represents a list of initial-lectures
-}

initialLecture :: String -> [Students] -> [String] ->  Lecture
initialLecture name students equipment =
  Lecture name allRooms [] students [NN] equipment

{- |
  'timetable' represents a list of all initial-lectures without a room
  and a lecturer before planingprozess
-}

timetable :: [Lecture]
timetable =
  [initialLecture "Prog1" [Students [Semester "BaMM1"] 60,
                           Students [Semester "BaIS1"] 25] ["Beamer"]
  ,initialLecture "VS" [Students [Semester "MaI3"] 30] ["Beamer"]
  ,initialLecture "WV" [Students [Semester "BaI3"] 50] ["Beamer"]
  ,initialLecture "AFS" [Students [Semester "BaI2"] 40] ["Beamer"]
  ,initialLecture "WV_Ue" [Students [Semester "BaMM1"] 60,
                          Students [Semester "BaIS1"] 25] ["Beamer"]
  ]

{- |
  'lectures' represents a list of all lectures with his lecturer
-}

lectures :: [(Lecturer,[String])]
lectures = [(Lecturer "Braun", ["Prog1","VS","WV_Ue"]),
            (Lecturer "Stiefel", ["WV","AFS","WV_Ue"])]

{- |
  'wishList' represents a list with wish-times of every lecturer
-}

wishList :: [(Lecturer, [(Day, [Time])])]
wishList =
  [(Lecturer "Braun", [(Monday, [Slot1,Slot2]), (Tuesday, [Slot3])])
  ,(Lecturer "Stiefel", [(Tuesday, [Slot1 .. Slot8])])]


