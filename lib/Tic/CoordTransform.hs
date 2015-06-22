module Tic.CoordTransform where

import           ClassyPrelude 
import Tic.Coordinate
import Linear.V2
import Control.Lens

-- TODO: Define an ISO, it's a classic example
coordToVector :: Coordinate a -> V2 a
coordToVector c = V2 (c ^. latitude) (c ^. longitude)

coordinateTransform :: (Fractional a,Eq a) => Coordinate a -> V2 a -> V2 a
coordinateTransform coord imageSize = coordToVector coord / V2 360 180 * imageSize + (imageSize / 2)
    
    
