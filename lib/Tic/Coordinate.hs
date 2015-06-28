{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
module Tic.Coordinate where

import           Control.Lens (makeLenses)
import ClassyPrelude

data Coordinate a = Coordinate {
    _latitude :: a
  , _longitude :: a
  } deriving(Functor)

deriving instance Show a => Show (Coordinate a)

$(makeLenses ''Coordinate)
