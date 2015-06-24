{-# LANGUAGE TemplateHaskell #-}
module Tic.GameState where

import Tic.Level
import Tic.Location
import Tic.Score
import Wrench.Time
import Control.Lens(makeLenses)

data GameState = GameState {
    _gsCurrentLevel :: Level
  , _gsTimerInited :: TimeTicks
  , _gsLocationSequence :: [Location]
  , _gsScore :: Score
  }

$(makeLenses ''GameState)
