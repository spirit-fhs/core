module Data.Time where

{- |
  'Time' represents all 8 time-slots for a lecture at FHS on one day
-}

data Time = Slot1 | Slot2 | Slot3 | Slot4 | Slot5 | Slot6 | Slot7 | Slot8
  deriving (Enum,Eq)

instance Show Time where
  show Slot1 = "08:15-09:45"
  show Slot2 = "10:00-11:30"
  show Slot3 = "11:45-13:15"
  show Slot4 = "14:15-15:45"
  show Slot5 = "16:00-17:30"
  show Slot6 = "17:45-19:15"
  show Slot7 = "19:30-21:00"
  show Slot8 = "21:15-22:45"
