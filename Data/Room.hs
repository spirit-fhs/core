module Data.Room where

{- |
  'Room' represents a room for a lecture with all his features like
  building, number, size and equipment
-}

data Room = Room {building :: String
                 ,number :: String
                 ,roomSize :: Int
                 ,roomEquipment :: [String]
                 }
  deriving (Show,Eq)
