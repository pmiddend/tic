module Tic.CoordTransform where

import           ClassyPrelude 
import Tic.Coordinate
import Linear.V2
import Control.Lens

coordToVector :: Coordinate a -> V2 a
coordToVector c = V2 (c ^. latitude) (c ^. longitude)

coordinateToPixel :: (Fractional a,Eq a) => Coordinate a -> V2 a -> V2 a
coordinateToPixel coord imageSize =
  coordToVector coord / V2 360 180 * imageSize + (imageSize / 2)
    
    
