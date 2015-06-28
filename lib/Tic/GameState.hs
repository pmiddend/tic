{-# LANGUAGE TemplateHaskell #-}
module Tic.GameState where

import Tic.Level
import Tic.Location
import Tic.Score
import Wrench.Time
import Control.Lens(makeClassy)

data GameState = GameState {
    _gsCurrentLevel :: Level
  , _gsTimerInited :: TimeTicks
  , _gsLocationSequence :: [Location]
  , _gsLevelScore :: Score
  , _gsTotalScore :: Score
  }

$(makeClassy ''GameState)
