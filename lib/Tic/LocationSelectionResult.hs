{-# LANGUAGE TemplateHaskell #-}
module Tic.LocationSelectionResult where

import           ClassyPrelude
import           Control.Lens  (makePrisms)
import           Tic.GeoCoord

data LocationSelectionResult a = LocationClicked (GeoCoord a)
                               | LocationTimedOut
                               deriving(Show)

$(makePrisms ''LocationSelectionResult)
