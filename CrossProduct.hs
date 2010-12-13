module CrossProduct where

import Data.List

{- |
  'crossProduct' gets two lists and returns the cross product as tuple-list
-}

crossProduct :: [a] -> [b] -> [(a,b)]
crossProduct xs ys = [ (x,y) | x <- xs, y <- ys]

{- |
  'crossProductAsList' gets two lists and returns the cross product as list
  of lists
-}

crossProductAsList :: [t] -> [t] -> [[t]]
crossProductAsList xs ys = map (\(x,y) -> [x,y]) $ crossProduct xs ys

{- |
  'crossProductAsSecond' gets a list of lists and a list and returns the
  cross product
-}

crossProductAsSecond :: [[t]] -> [t] -> [[t]]
crossProductAsSecond xss ys = [ xs ++ [y] | xs <- xss, y <- ys ]

{- |
  'crossProductOverList' gets a list of lists and returns the cross product
  of the list
-}

crossProductOverList :: [[t]] -> [[t]]
crossProductOverList [] = []
crossProductOverList xs@(_:[]) = xs
crossProductOverList (x:y:ys) =
  foldl crossProductAsSecond (crossProductAsList x y) ys


