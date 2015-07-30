{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE TemplateHaskell #-}
module Tic.HudPictureInput where

import           ClassyPrelude
import           Control.Lens  (makeLenses)

data HudPictureInput a = HudPictureInput {
    _hpiLeftText      :: Maybe Text
  , _hpiCenterText    :: Maybe Text
  , _hpiRightText     :: Maybe Text
  , _hpiPercentFilled :: Maybe a
  } deriving(Functor)

$(makeLenses ''HudPictureInput)
