{-# LANGUAGE TemplateHaskell #-}
module Tic.HudPictureInput where

import ClassyPrelude
import Wrench.FloatType
import Control.Lens(makeLenses)

data HudPictureInput = HudPictureInput {
    _hpiLeftText :: Maybe Text
  , _hpiCenterText :: Maybe Text
  , _hpiRightText :: Maybe Text
  , _hpiPercentFilled :: Maybe FloatType
  }

$(makeLenses ''HudPictureInput)
