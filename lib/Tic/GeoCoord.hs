{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
module Tic.GeoCoord where

import           Control.Lens (makeLenses)
import ClassyPrelude

data GeoCoord a = GeoCoord {
    _latitude :: a
  , _longitude :: a
  } deriving(Functor)

deriving instance Show a => Show (GeoCoord a)
deriving instance Eq a => Eq (GeoCoord a)

$(makeLenses ''GeoCoord)
