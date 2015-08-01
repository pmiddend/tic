{-# LANGUAGE TemplateHaskell #-}
module Tic.Level where

import           ClassyPrelude
import           Control.Lens  (makePrisms)

newtype Level = Level Int deriving(Show)

$(makePrisms ''Level)
