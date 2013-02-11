module Main where

import System.IO
import System.Exit
import Data.Lectures
import Data.DB
import Timetabling


{- | 
  'main' is the main-function of spiritcore
-}

main :: IO ()
main = do
  let loL = mkListOfLectures timetable lectures wishList allRooms
  let allOneLP = mkAllOneLecturePossibilities startLectureList
  let sortBySS = sortByStudents allOneLP
  let sortByRELP = sortByRoomEconomy sortBySS
  let all = allTimetables startLectureList

  mainloop loL allOneLP sortByRELP sortBySS all True


{- | 
  'mainloop' is the mainloop-function with the menu
-}

mainloop :: (Show a1, Show a2, Show a3, Show a4, Eq a, Show a) =>
                 a1 -> a2 -> a4 -> a3 -> [a] -> Bool -> IO ()
mainloop loL allOneLP sortByRELP sortBySS all status
  | status == False = do putStrLn "bye bye"
                         exitWith ExitSuccess
  | status == True = 
   do
     putStrLn "\nThis is Spirit-Core"
     putStrLn "==================="
     putStrLn "\nPleace select a Action:"
     putStrLn "-----------------------"
     putStrLn "[1] List of all Lectures"
     putStrLn "[2] List of all OneLecture-Lists"
     putStrLn "[3] OneLecture-List after sortByStudents - size"
     putStrLn "[4] OneLecture-List after sortByRoomEconomy"
     putStrLn "[5] check possibilities in crossProductLectureList"
     putStrLn "[6] count all possible timetables"
     putStrLn "[7] One Timetable"
     putStrLn "[0] exit"
     putStrLn "-----------------------"
  
     input <- getLine
  
     checkInput input
 
     mainloop loL allOneLP sortByRELP sortBySS all status
  | otherwise = error "error!"
  
    where checkInput :: String -> IO ()
          checkInput input 
            | input == "0" = 
              do putStrLn "shutdown now"
                 mainloop  loL allOneLP sortByRELP sortBySS all False
            | input == "1" = 
              do putStrLn $ "\nList of all Lectures" ++
                            "\n--------------------"
                 print loL
                 mainloop loL allOneLP sortByRELP sortBySS all status
            | input == "2" = 
              do putStrLn $ "\nList of all OneLecture-Lists" ++
                            "\n----------------------------"
                 print allOneLP
                 mainloop loL allOneLP sortByRELP sortBySS all status
            | input == "3" = 
              do putStrLn $ "\nsorteByStudents - size" ++
                            "\n-----------------------"
                 print sortBySS 
                 mainloop loL allOneLP sortByRELP sortBySS all status
            | input == "4" = 
              do putStrLn $ "\nsortByRoomEconomy" ++
                            "\n-----------------"
                 print sortByRELP 
                 mainloop loL allOneLP sortByRELP sortBySS all status
            | input == "5" =
              do putStrLn $ "\npossibilities in corssProductLectureList" ++
                            "\n----------------------------------------" 
                 print $ map checkRDTSL $ crossProductLectureList 
                                          startLectureList
                 mainloop loL allOneLP sortByRELP sortBySS all status
            | input == "6" =
              do putStrLn $ "\ncount all possible timetables" ++
                            "\n-----------------------------"
                 putStrLn $ (show $ length all)  ++ " timetables counted"
                 mainloop loL allOneLP sortByRELP sortBySS all status
            | input == "7" =
              getOneTimetable all "n"
                 
            | otherwise = putStrLn "wrong input!"   
    
          getOneTimetable all stat
            | all == [] = putStrLn "no timetable found"
            | stat == "0" = putStrLn "back"
            | stat == "n" = do putStrLn $ "\nOne Timetable:\n" ++
                                          "--------------\n\n"
                               print $ head all
                               putStrLn $ "\npress n for the next timetable " ++
                                          "or 0 to leave"
                               putStrLn $ "-------------------------------" ++
                                          "-------------"
                               input <- getLine
                               getOneTimetable (tail all) input
            | otherwise = putStrLn "wrong input!" 
                               





                                
                                  
           

 



