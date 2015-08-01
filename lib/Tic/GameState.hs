{-# LANGUAGE TemplateHaskell #-}
module Tic.GameState where

import           Control.Lens (makeClassy)
import           Tic.Level
import           Tic.Location
import           Tic.Score
import           Tic.UnitType
import           Wrench.Time

data GameState = GameState {
    _gsCurrentLevel     :: Level
  , _gsTimerInited      :: TimeTicks
  , _gsLocationSequence :: [Location UnitType]
  , _gsLevelScore       :: Score
  , _gsTotalScore       :: Score
  }

$(makeClassy ''GameState)
