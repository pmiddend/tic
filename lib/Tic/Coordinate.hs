{-# LANGUAGE TemplateHaskell #-}
module Tic.Coordinate where

import           Control.Lens (makeLenses)

data Coordinate a = Coordinate {
    _latitude :: a
  , _longitude :: a
  }

$(makeLenses ''Coordinate)
