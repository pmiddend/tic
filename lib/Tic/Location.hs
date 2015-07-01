{-# LANGUAGE TemplateHaskell #-}
module Tic.Location where

import Tic.GeoCoord
import ClassyPrelude
import Wrench.FloatType
import Control.Lens(makeLenses)

data Location = Location {
    _locName :: Text
  , _locCoord :: GeoCoord FloatType
  } deriving(Show)

$(makeLenses ''Location)

