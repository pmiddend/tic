{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
module Tic.ViewportCoord where

import Control.Lens(makeLenses)
import ClassyPrelude

data ViewportCoord a = ViewportCoord {
    _vpx :: a
  , _vpy :: a
  } deriving(Functor)

deriving instance Eq a => Eq (ViewportCoord a)
deriving instance Show a => Show (ViewportCoord a)

$(makeLenses ''ViewportCoord)
