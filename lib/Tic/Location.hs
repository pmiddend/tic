{-# LANGUAGE TemplateHaskell #-}
module Tic.Location where

import Tic.Coordinate
import ClassyPrelude
import Wrench.FloatType
import Control.Lens(makeLenses)

data Location = Location {
    _locName :: Text
  , _locCoordinate :: Coordinate FloatType
  } deriving(Show)

$(makeLenses ''Location)

