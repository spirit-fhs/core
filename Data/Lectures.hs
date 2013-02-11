module Data.Lectures where

import Data.List
import Data.Room
import Data.Day
import Data.Time
import Data.Semester
import Data.Students
import Data.Lecturer
import Data.Lecture
import Data.Timetable
import Data.DB

{-
  The module Data.Lectures creates a list of timetables for the
  planing-prozess
-}

{- |
  'withLecturer' adds the lecturer to a inital-lecture
-}

withLecturer :: [(Lecturer,[String])] -> Lecture -> Lecture
withLecturer lectures (Lecture name rooms timeslots students _ equipment) =
  let lecturerL = filter (elem name . snd) lectures
  in if lecturerL == []
     then error (name ++ " not assigned to Lecturer")
     else Lecture name
             rooms
             timeslots
             students
             (map fst lecturerL)
             equipment


{- |
  'withWishList' adds a list of possible timeslots to a Lecture
-}

withWishList :: [(Lecturer, [(Day, [Time])])] -> Lecture -> Lecture
withWishList wishList (Lecture name rooms _ students lecturer  equipment) =
  let timeslotL = map snd (filter (flip elem lecturer . fst) wishList)
  in if timeslotL == []
     then error ("Lecturer for " ++  name ++ " does not exist in wishList")
     else Lecture name
             rooms
             (jointSlots timeslotL)
             students
             lecturer
             equipment

  where
    jointTwoSlots :: [(Day, [Time])] ->[(Day, [Time])] -> [(Day, [Time])]
    jointTwoSlots xs ys = [ (dayX, [ timeX
                                   | timeX <- timesX
                                   , timeY <- timesY
                                   , timeX == timeY
                                   ])
                          | (dayX,timesX) <- xs
                          , (dayY,timesY) <- ys
                          , dayX == dayY
                          ]

    allSlots = zip [Monday .. Friday] $ repeat [Slot1 .. Slot8]

    jointSlots :: [[(Day, [Time])]] -> [(Day, [Time])]
    jointSlots = foldr jointTwoSlots allSlots


{- |
  'sizeOfRoomForLecture' calculates the size of a list of studenst
-}

sizeOfRoomForLecture :: Lecture -> Students
sizeOfRoomForLecture (Lecture _ _ _ students _ _) =
  foldl (+)(Students [] 0) students

{- |
  'withAvailableRooms' adds a list of rooms to a lecture with the right
  size and equipment
-}

withAvailableRooms :: [Room] -> Lecture -> Lecture
withAvailableRooms allRooms (Lecture name
                                     roomL
                                     timeslots
                                     students
                                     lecturer
                                     equipment)

  = let rooms = filter (\(Room _ _  size _) ->
                         Students [Semester ""] size >=
                         sizeOfRoomForLecture (Lecture name
                                                       roomL
                                                       timeslots
                                                       students
                                                       lecturer
                                                       equipment)
                       ) allRooms
    in if rooms == []
       then error (name ++ " is to Big!")
       else let roomL = filter (\(Room _ _ _ eq) ->
                     intersect eq equipment == equipment) rooms
            in if roomL == []
               then error ("No room with the right equipment for " ++ name)
               else (Lecture name roomL
                                  timeslots
                                  students
                                  lecturer
                                  equipment)


{- |
  'mkListOfLectures' gets a list of inital-lectures and creates a list of
  lectures with possible timeslost and rooms for the planing-prozess
-}

mkListOfLectures :: [Lecture] ->
                    [(Lecturer,[String])] ->
                    [(Lecturer, [(Day, [Time])])] ->
                    [Room] ->
                    [Lecture]
mkListOfLectures timetable lectures wishList allRooms =
  map (withAvailableRooms allRooms .
       withWishList wishList .
       withLecturer lectures) timetable



