module Main where

import Wrench.MonadGame
import Wrench.Platform
import ClassyPrelude 
import Wrench.Rectangle
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
      smallerRatio = inner ^. rectangleDimensions . _x / inner ^. rectangleDimensions . _y
      height = width * smallerRatio
      x = 0
      y = outer ^. rectangleDimensions . _y / 2 - height / 2
    in
      rectangleFromPoints (V2 x y) (V2 width height)
  else
    let
      height = outer ^. rectangleDimensions . _y
      smallerRatio = inner ^. rectangleDimensions . _y / inner ^. rectangleDimensions . _x
      width = height * smallerRatio
      x = outer ^. rectangleDimensions . _x / 2 - width / 2
      y = 0
    in
      rectangleFromPoints (V2 0 0) (V2 width height)


mapWidth = 3600

mapHeight = 1800

mainLoop = do
  events <- gpollEvents
  gupdateTicks 1.0
  gupdateKeydowns events
  mainLoop

main :: IO ()
main = runGame "media" "tic 0.1" DynamicWindowSize mainLoop
