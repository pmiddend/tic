{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
module Tic.GeoCoord where

import           ClassyPrelude
import           Control.Lens  (makeLenses)

data GeoCoord a = GeoCoord {
    _latitude  :: a
  , _longitude :: a
  } deriving(Functor)

deriving instance Show a => Show (GeoCoord a)
deriving instance Eq a => Eq (GeoCoord a)

$(makeLenses ''GeoCoord)
