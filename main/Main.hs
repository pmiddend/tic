module Main where

import Wrench.MonadGame
import Wrench.SpriteIdentifier
import Wrench.Point
import Wrench.Platform
import Wrench.Event
import Wrench.Picture
import ClassyPrelude 
import Wrench.Rectangle
import Data.Maybe
import Wrench.RenderPositionMode
import Control.Lens((^.),(^..))
import Linear.V2

type OuterRectangle = Rectangle
type InnerRectangle = Rectangle

fitRectAspectPreserving :: OuterRectangle -> InnerRectangle -> Rectangle
fitRectAspectPreserving outer inner =
  if inner ^. rectangleDimensions . _x > inner ^. rectangleDimensions . _y
  then fitRectAspectPreserving' (outer ^. rectangleDimensions) (inner ^. rectangleDimensions)
  else fitRectAspectPreserving' (outer ^. rectangleDimensions . _yx) (inner ^. rectangleDimensions . _yx)
  where 
    fitRectAspectPreserving' :: Point -> Point -> Rectangle
    fitRectAspectPreserving' outer' inner' = 
        let
            width = outer' ^. _x
            smallerRatio = inner' ^. _y / inner' ^. _x
            height = width * smallerRatio
            x = 0
            y = outer' ^. _y / 2 - height / 2
        in
            rectangleFromOriginAndDim (V2 x y) (V2 width height)

mapImageId :: SpriteIdentifier
mapImageId = "map"

originRectangle :: Wrench.Point.Point -> Rectangle
originRectangle = rectangleFromPoints (V2 0 0)

fitMap :: OuterRectangle -> InnerRectangle -> Rectangle
fitMap viewportRectangle mapSize = fitRectAspectPreserving viewportRectangle mapSize

picturize :: OuterRectangle -> InnerRectangle -> Picture
picturize viewportRectangle mapSize =
  let fitRect = fitMap viewportRectangle mapSize
  in (fitRect ^. rectLeftTop) `pictureTranslated` (pictureSpriteResampled mapImageId RenderPositionTopLeft (fitRect ^. rectangleDimensions))

type MouseCoord = Point
type MapRectangle = Rectangle

mouseCoordToImageCoord :: MouseCoord -> MapRectangle -> Maybe Point
mouseCoordToImageCoord mouse image = undefined

mainLoop = do
  events <- gpollEvents
  let positions = events ^.. traverse . _MouseButton . mousePosition
  when (not . null $ positions) (print positions)
  gupdateTicks 1.0
  gupdateKeydowns events
  viewport <- originRectangle <$> gviewportSize
  mapImage <- fromJust <$> (glookupImageRectangle mapImageId)
  let fitRect = fitMap viewport mapImage
  grender (picturize viewport mapImage)
  mainLoop

main :: IO ()
main = runGame "media" "tic 0.1" DynamicWindowSize mainLoop
