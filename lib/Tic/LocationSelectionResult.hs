{-# LANGUAGE TemplateHaskell #-}
module Tic.LocationSelectionResult where

import Control.Lens(makePrisms)
import Wrench.FloatType
import Tic.GeoCoord
import ClassyPrelude

data LocationSelectionResult = LocationClicked (GeoCoord FloatType)
                             | LocationTimedOut
                             deriving(Show)

$(makePrisms ''LocationSelectionResult)
