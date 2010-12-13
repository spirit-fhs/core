module Data.Semester where

{- |
  'Semester' represents all semester-groups of the FHS
-}

data Semester = Semester {group :: String}
  deriving (Show,Eq,Ord)
