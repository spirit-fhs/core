module Timetabling where

import Data.Lectures
import Data.DB
import Data.Lecture
import Data.OneLecture
import Data.Students

import Data.Day
import Data.Time
import Data.Room
import CrossProduct
import Data.List
import Data.AllTimetables

{-
  The module Timetabeling calculates all valid timetables on the basis of
  the knowledge base in module Data.DB
-}


--------------- edit plan-data

{- |
  'mkDayTime' gets a tuble of a day and a list of timeslots and returns
  a list of tubles with a day and one timeslot
-}

mkDayTime :: (Day,[Time]) -> [(Day,Time)]
mkDayTime (day,times) = map (\time -> (day, time)) times

{- |
  'mkDayTimeList' gets a list of day-times-tubles and returns a list of
  all possible day-time-tubles
-}

mkDayTimeList :: [(Day,[Time])] -> [[(Day,Time)]]
mkDayTimeList = map mkDayTime

{- |
  'mkDayTimeRoom' gets a list of rooms and a list of day-time-tubles and
  returns the crossProduct of room-day-time-tubles
-}

mkDayTimeRoom :: [Room] -> [(Day,Time)] -> [(Room,(Day,Time))]
mkDayTimeRoom rooms dayTime = crossProduct rooms dayTime

{- |
  'mkDayTimeRooList' gets a list of rooms and a list of day-times-tubles
  and returns a union of room-day-time-tubles
-}

mkDayTimeRoomList :: [Room] -> [(Day,[Time])] -> [(Room,(Day,Time))]
mkDayTimeRoomList rooms timeslots =
  concatMap (mkDayTimeRoom rooms) $ mkDayTimeList timeslots


--Test mkOneLectureList (head startLectureList )

{- |
  'mkOneLectureList' gets a Lecture and returns a list of all possible
  OneLectures from this Lecture
-}

mkOneLectureList :: Lecture -> [OneLecture]
mkOneLectureList (Lecture name rooms timeslots students lecturer _) =
  let dtrL = mkDayTimeRoomList rooms timeslots
  in map (\(room,(day,time)) -> OneLecture name
                                           room
                                           day
                                           time
                                           students
                                           lecturer) dtrL

------------------------- start planing

--step 1

{- |
  'startLectureList' is a list of all lectures which is needed to
  calculate a timetable
-}

startLectureList :: [Lecture]
startLectureList =
  mkListOfLectures timetable lectures wishList allRooms


--step 2

{- |
  'mkAllOneLecturePossibilities' converts lectures to a list of lists
  with all possibilites for one lecture
-}

mkAllOneLecturePossibilities :: [Lecture] -> [[OneLecture]]
mkAllOneLecturePossibilities startL = map mkOneLectureList startL

{- |
  'sortByStudents' sorts a list of OneLectures descendant by the size of
  the students
-}

sortByStudents :: [[OneLecture]] -> [[OneLecture]]
sortByStudents lectures = reverse $ sort lectures

{- |
  'sortByRoomEconomy' sorts the OneLecture-lists in a list by room-economy
-}

sortByRoomEconomy :: [[OneLecture]] -> [[OneLecture]]
sortByRoomEconomy lectures =
  map (sortBy (\a b -> compare (diffRoomStudents a) (diffRoomStudents b))) lectures

{- |
  'diffRoomStudents' calculates the difference between the room and students
  size
-}


diffRoomStudents :: OneLecture -> Int
diffRoomStudents (OneLecture _ (Room _ _ size _ ) _ _ students  _) =
  let sizeOfStudents = foldl (+)(Students [] 0) students
      diff (Students _ count) roomSize = roomSize - count
  in diff sizeOfStudents size


{- |
  'sortedLectureList' returns a list of OneLectures after sort by
  students-size and room-economy
-}

sortedLectureList :: [Lecture] -> [[OneLecture]]
sortedLectureList =
  sortByStudents . sortByRoomEconomy . mkAllOneLecturePossibilities

--step 3

{- |
  'crossProductLectureList' returns the cross product of the
  sortedLectureList
-}

crossProductLectureList :: [Lecture] -> [[OneLecture]]
crossProductLectureList = crossProductOverList . sortedLectureList

--step 4

{- |
  'checkRDTSL' gets a timetable and calculates the validity of this timetable. 
  If checkRDTSL finds a clash between room, day, time, students-group and 
  lecturer the funktion will return False.
-}

checkRDTSL :: [OneLecture] -> Bool
checkRDTSL [] = True
checkRDTSL
  (x@(OneLecture  n
   (Room building number si e) day time s l):xs)
  | eqRDTSL x xs = False
  | otherwise = checkRDTSL xs
  where eqRDTSL (OneLecture n (
                 Room bb number si e) day time st le) list
          = if filter (\(OneLecture _ (Room b nr _ _) dd tt ss ll) ->
                          (bb == b &&
                           number == nr &&
                           day == dd &&
                           time == tt) ||
                          (day == dd &&
                           time == tt &&
                           ((intersect st ss) /= [])) ||
                          (day == dd &&
                           time == tt &&
                           ((intersect le ll) /= []))
                       ) list == []
            then False
            else True

{- |
  'allTimetables' represents a list of all valid timetables
-}

allTimetables :: [Lecture] -> AllTimetables
allTimetables lectures =
  let lectures' = crossProductLectureList lectures
  in filter checkRDTSL lectures'

