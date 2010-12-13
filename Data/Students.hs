module Data.Students where

import Data.Semester

{- |
  'Students' represents a group of semester-groups which take part in
  a lecture
-}

data Students = Students { semester :: [Semester]
                         , count :: Int
                         }
  deriving (Show,Eq)

instance Num Students where
  Students a b + Students c d = Students (a++c) (b + d)
  Students a b * Students c d = Students (a++c) (b * d)
  abs = id
  signum = const 1
  fromInteger = Students [] . fromInteger

instance Ord Students where
  Students a x <= Students b y  =  compare x y /= GT
  Students a x < Students b y   =  compare x y == LT
  Students a x >= Students b y  =  compare x y /= LT
  Students a x >  Students b y  =  compare x y == GT
