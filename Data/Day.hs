module Data.Day where

{- |
  'Day' represents a day in a normal study-week
-}

data Day = Monday | Tuesday | Wednesday | Thursday | Friday
  deriving (Show,Eq,Enum)

