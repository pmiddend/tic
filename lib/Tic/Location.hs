{-# LANGUAGE TemplateHaskell #-}
module Tic.Location where

import           ClassyPrelude
import           Control.Lens  (makeLenses)
import           Tic.GeoCoord

data Location a = Location {
    _locName  :: Text
  , _locCoord :: GeoCoord a
  } deriving(Show)

$(makeLenses ''Location)

