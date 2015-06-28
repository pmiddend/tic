module Tic.CoordTransform where

import           ClassyPrelude 
import Tic.Coordinate
import Linear.V2
import Control.Lens((^.),from,iso,Iso')
import Numeric.Lens
import Data.Profunctor

toVector :: Iso' (Coordinate a) (V2 a)
toVector = iso (\c -> V2 (c ^. latitude) (c ^. longitude)) (\v -> Coordinate (v ^. _x) (v ^. _y))

coordToVector :: Coordinate a -> V2 a
coordToVector c = V2 (c ^. latitude) (c ^. longitude)

toRadians :: Floating a => a -> a
toRadians = (/180) . (*pi)

distanceHaversine :: (Floating a,RealFloat a) => Coordinate a -> Coordinate a -> a
distanceHaversine ca cb =
  let
    lata = toRadians (ca ^. latitude)
    latb = toRadians (cb ^. latitude)
    latd = toRadians $ latb - lata
    lond = toRadians $ (cb ^. longitude) - (ca ^. longitude)
    r = 6371000
    a = (sin (latd/2))**2 + cos lata * cos latb * (lond/2)**2
    c = 2 * atan2 (sqrt a) (sqrt (1-a))
  in
    r * c

coordinateToPixelLens :: (Profunctor p, Functor f, Fractional a, Eq a) => V2 a -> p (V2 a) (f (V2 a)) -> p (Coordinate a) (f (Coordinate a))
coordinateToPixelLens imageSize = toVector . dividing (V2 360 180) . multiplying imageSize . adding (imageSize / 2)

coordinateToPixel :: (Fractional a,Eq a) => Coordinate a -> V2 a -> V2 a
coordinateToPixel coord imageSize = coord ^. (coordinateToPixelLens imageSize)

pixelToCoordinate :: (Fractional a,Eq a) => V2 a -> V2 a -> Coordinate a
pixelToCoordinate coord imageSize = coord ^. from (coordinateToPixelLens imageSize)
    
    
