{-# LANGUAGE TemplateHaskell #-}
module Tic.Location where

import           ClassyPrelude
import           Control.Lens  (makeLenses)
import           Tic.GeoCoord
import           Tic.UnitType

data Location = Location {
    _locName  :: Text
  , _locCoord :: GeoCoord UnitType
  } deriving(Show)

$(makeLenses ''Location)

