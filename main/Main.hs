module Main where

import Wrench.MonadGame
import Wrench.SpriteIdentifier
import Wrench.Point
import Wrench.Platform
import Wrench.Picture
import ClassyPrelude 
import Wrench.Rectangle
import Data.Maybe
import Wrench.RenderPositionMode
import Control.Lens((^.))
import Linear.V2

type OuterRectangle = Rectangle
type InnerRectangle = Rectangle

fitRectAspectPreserving :: OuterRectangle -> InnerRectangle -> Rectangle
fitRectAspectPreserving outer inner =
  if inner ^. rectangleDimensions . _x > inner ^. rectangleDimensions . _y
  then
    let
      width = outer ^. rectangleDimensions . _x
      smallerRatio = inner ^. rectangleDimensions . _y / inner ^. rectangleDimensions . _x
      height = width * smallerRatio
      x = 0
      y = outer ^. rectangleDimensions . _y / 2 - height / 2
    in
      rectangleFromOriginAndDim (V2 x y) (V2 width height)
  else
    let
      height = outer ^. rectangleDimensions . _y
      smallerRatio = inner ^. rectangleDimensions . _x / inner ^. rectangleDimensions . _y
      width = height * smallerRatio
      x = outer ^. rectangleDimensions . _x / 2 - width / 2
      y = 0
    in
      rectangleFromOriginAndDim (V2 x y) (V2 width height)

mapImageId :: SpriteIdentifier
mapImageId = "map"

originRectangle :: Wrench.Point.Point -> Rectangle
originRectangle = rectangleFromPoints (V2 0 0)

picturize :: OuterRectangle -> InnerRectangle -> Picture
picturize viewportRectangle mapSize =
  let fitRect = fitRectAspectPreserving viewportRectangle mapSize
  in (fitRect ^. rectLeftTop) `pictureTranslated` (pictureSpriteResampled mapImageId RenderPositionTopLeft (fitRect ^. rectangleDimensions))

mainLoop = do
  events <- gpollEvents
  gupdateTicks 1.0
  gupdateKeydowns events
  viewport <- gviewportSize
  mapImage <- fromJust <$> (glookupImageRectangle mapImageId)
  grender (picturize (originRectangle viewport) mapImage)
  mainLoop

main :: IO ()
main = runGame "media" "tic 0.1" DynamicWindowSize mainLoop
