{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
module Tic.ViewportCoord where

import           ClassyPrelude
import           Control.Lens  (makeLenses)

data ViewportCoord a = ViewportCoord {
    _vpx :: a
  , _vpy :: a
  } deriving(Functor)

deriving instance Eq a => Eq (ViewportCoord a)
deriving instance Show a => Show (ViewportCoord a)

$(makeLenses ''ViewportCoord)
