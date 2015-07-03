{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
module Tic.ImageCoord where

import Control.Lens(makeLenses)
import ClassyPrelude

data ImageCoord a = ImageCoord {
    _imx :: a
  , _imy :: a
  } deriving(Functor)

deriving instance Eq a => Eq (ImageCoord a)
deriving instance Show a => Show (ImageCoord a)

$(makeLenses ''ImageCoord)
