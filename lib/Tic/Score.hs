{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Tic.Score where

import           ClassyPrelude

newtype Score = Score Int deriving(Integral,Real,Enum,Ord,Num,Eq,Show)


