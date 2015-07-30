{-# LANGUAGE TemplateHaskell #-}
module Tic.LocationSelectionResult where

import           ClassyPrelude
import           Control.Lens  (makePrisms)
import           Tic.GeoCoord
import           Tic.UnitType

data LocationSelectionResult = LocationClicked (GeoCoord UnitType)
                             | LocationTimedOut
                             deriving(Show)

$(makePrisms ''LocationSelectionResult)
