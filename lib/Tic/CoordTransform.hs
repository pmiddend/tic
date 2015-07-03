{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Tic.CoordTransform where

import ClassyPrelude 
import Tic.GeoCoord
import Tic.ImageCoord
import Tic.ViewportCoord
import Wrench.Rectangle
import Wrench.FloatType
import Wrench.CommonGeometry
import Linear.V2
import Control.Lens((^.),from,iso,Iso',to,Getter)
import Numeric.Lens

geoToVector :: Iso' (GeoCoord a) (V2 a)
geoToVector = iso (\c -> V2 (c ^. longitude) (c ^. latitude)) (\v -> GeoCoord (v ^. _y) (v ^. _x))

imageToVector :: Iso' (ImageCoord a) (V2 a)
imageToVector = iso (\c -> V2 (c ^. imx) (c ^. imy)) (\v -> ImageCoord (v ^. _x) (v ^. _y))

viewportToVector :: Iso' (ViewportCoord a) (V2 a)
viewportToVector = iso (\c -> V2 (c ^. vpx) (c ^. vpy)) (\v -> ViewportCoord (v ^. _x) (v ^. _y))

toRadians :: (Floating a,Eq a) => Iso' a a
toRadians = (dividing 180) . (multiplying pi)

distanceHaversine :: (Floating a,RealFloat a) => GeoCoord a -> GeoCoord a -> a
distanceHaversine ca cb =
  let
    lata = (ca ^. latitude . toRadians)
    latb = (cb ^. latitude . toRadians)
    latd = (latb - lata) ^. toRadians
    lond = ((cb ^. longitude) - (ca ^. longitude)) ^. toRadians
    r = 6371000
    a = (sin (latd/2))**2 + cos lata * cos latb * (lond/2)**2
    c = 2 * atan2 (sqrt a) (sqrt (1-a))
  in
    r * c

geoVectorYNormalize :: (Eq a,Floating a) => Iso' a a
geoVectorYNormalize = dividing 180 . negated . adding 0.5

vectorIso :: Iso' a a -> Iso' a a -> Iso' (V2 a) (V2 a)
vectorIso xlens ylens = iso (\(V2 a b) -> V2 (a ^. xlens) (b ^. ylens)) (\(V2 a b) -> V2 (a ^. from xlens) (b ^. from ylens))

geoVectorNormalize :: forall a.Fractional a => Eq a => Floating a => Iso' (V2 a) (V2 a)
geoVectorNormalize = vectorIso (dividing 360 . adding 0.5) geoVectorYNormalize

geoCoordToImageCoord :: (Floating a,Fractional a,Num a,Eq a) => V2 a -> Iso' (GeoCoord a) (ImageCoord a)
geoCoordToImageCoord imageSize = geoToVector . geoVectorNormalize . multiplying imageSize . from imageToVector

imageCoordToGeoCoord :: (Floating a,Fractional a,Num a,Eq a) => V2 a -> Iso' (ImageCoord a) (GeoCoord a) 
imageCoordToGeoCoord imageSize = from (geoCoordToImageCoord imageSize)

imageCoordToViewportCoord :: Num a => V2 a -> Iso' (ImageCoord a) (ViewportCoord a) 
imageCoordToViewportCoord imageOrigin = imageToVector . adding imageOrigin . from viewportToVector

--viewportCoordToImageCoord imageOrigin = from (imageCoordToViewportCoord imageOrigin)
viewportCoordToImageCoord :: Rectangle -> Getter (ViewportCoord FloatType) (Maybe (ImageCoord FloatType))
viewportCoordToImageCoord imageRectangle = to helper
  where helper vc | pointInRectangle (vc ^. viewportToVector) imageRectangle = Just ((vc ^. viewportToVector - (imageRectangle ^. rectLeftTop)) ^. from imageToVector)                                                  
                  | otherwise = Nothing
