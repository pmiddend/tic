{-# LANGUAGE FlexibleContexts #-}
module Main where

import Wrench.MonadGame
import Wrench.SpriteIdentifier
import Wrench.Point
import Wrench.Platform
import Wrench.Time
import Wrench.FloatType
import System.Random.Shuffle(shuffleM)
import Wrench.Event
import Control.Monad.Random
import Wrench.Event
import Wrench.Picture
import Wrench.MouseButtonMovement
import ClassyPrelude hiding(head)
import Wrench.Rectangle
import Wrench.CommonGeometry
import Data.Maybe
import Control.Monad.State.Strict(MonadState,execStateT)
import Tic.Coordinate
import Tic.Level
import Tic.Score
import Tic.CoordTransform
import Tic.Location
import Wrench.RenderPositionMode
import Tic.GameState
import Control.Lens((^.),(^..),filtered,folding,ix,(^?!),use,(%=),(<~),(+=),_head,folded,to,singular,_Just,taking)
import Data.List(tail,head)
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

picturize :: Rectangle -> Picture
picturize fitRect = (fitRect ^. rectLeftTop) `pictureTranslated` (pictureSpriteResampled mapImageId RenderPositionTopLeft (fitRect ^. rectangleDimensions))

type MouseCoord = Point
type MapRectangle = Rectangle

toImageCoord :: MapRectangle -> Point -> Maybe Point
toImageCoord image mouse
  | pointInRectangle mouse image = Just (mouse - (image ^. rectLeftTop))
  | otherwise = Nothing

levelDatabase :: [[Location]]
levelDatabase = [
    [ Location "Hannover" (Coordinate 52.518611 13.408333)
    , Location "Osnabrück" (Coordinate 52.278889 8.043056)
    ]
  , [ Location "Belm" (Coordinate 52.3 8.133333)
    , Location "Mali" (Coordinate 17.0 4.366667)
    ]
  ]

chooseLocationSequence :: MonadRandom m => Level -> m [Location]
chooseLocationSequence level = do
  shuffled <- shuffleM (levelDatabase ^?! ix level)
  return (take (locationsPerLevel level) shuffled)

minScoreForLevel :: Level -> Score
minScoreForLevel = undefined

timerDurationForLevel :: Level -> TimeDelta
timerDurationForLevel = undefined

locationsPerLevel :: Level -> Int
locationsPerLevel = const 2

distanceAndTimeToPoints :: FloatType -> TimeDelta -> Score
distanceAndTimeToPoints = undefined

geoLocationDistance :: Coordinate FloatType -> Coordinate FloatType -> FloatType
geoLocationDistance = undefined

pixelDistance :: Point -> Point -> FloatType
pixelDistance = undefined

data GameAction = GameContinues
                | GameOver
                | GameNextLevel
                | GameNextSequenceItem

determineGameAction :: (MonadGame m,MonadState GameState m) => m GameAction
determineGameAction = do
  currentTicks <- gcurrentTicks
  timerInited <- use gsTimerInited
  currentLevel <- use gsCurrentLevel
  if currentTicks `tickDelta` timerInited > timerDurationForLevel currentLevel
    then do
      currentLocations <- use gsLocationSequence
      if length currentLocations == 1
      then do
        currentScore <- use gsScore
        if currentScore < minScoreForLevel currentLevel
        then return GameOver
        else return GameNextLevel
      else return GameNextSequenceItem
    else return GameContinues

timeLeft :: (Monad m,MonadGame m,MonadState GameState m) => m TimeDelta
timeLeft = do
  timerInited <- use gsTimerInited
  currentTicks <- gcurrentTicks
  return (currentTicks `tickDelta` timerInited)

processClick imageSize clickPosition = do
  currentLocation <- head <$> use gsLocationSequence
  let dist = pixelDistance (coordinateToPixel (currentLocation ^. locCoordinate) imageSize) clickPosition
  tl <- timeLeft
  gsScore += distanceAndTimeToPoints dist tl
  return ()

mainLoop :: (Monad m,Applicative m,Functor m,MonadIO m,MonadGame m,MonadState GameState m,MonadRandom m) => m ()
mainLoop = do
  events <- gpollEvents
  let positions = events ^.. traverse . _MouseButton . mousePosition
  when (not . null $ positions) (print positions)
  gupdateTicks 1.0
  viewport <- originRectangle <$> gviewportSize
  mapImage <- fromJust <$> (glookupImageRectangle mapImageId)
  let
    fitRect = fitMap viewport mapImage
    positions = events ^.. traverse . _MouseButton . filtered ((== ButtonDown) . (^. mouseButtonMovement)) . mousePosition . to (toImageCoord fitRect) . _Just . singular _head
  mapM_ (processClick (fitRect ^. rectangleDimensions)) positions
  let
    gameStatePicture = undefined
  grender (pictures [(picturize fitRect),gameStatePicture])
  gameAction <- determineGameAction
  case gameAction of
    GameContinues -> mainLoop
    GameOver -> do
      score <- use gsScore
      putStrLn $ "Your final score: " <> (pack . show) score
      return ()
    GameNextSequenceItem -> do
      gsLocationSequence %= tail
      gsTimerInited <~ gcurrentTicks
      mainLoop
    GameNextLevel -> do
      gsLocationSequence <~ (chooseLocationSequence =<< use gsCurrentLevel)
      gsTimerInited <~ gcurrentTicks
      mainLoop

main :: IO ()
main = do
  runGame "media" "tic 0.1" DynamicWindowSize $ do
    initialLocations <- chooseLocationSequence 0
    currentTicks <- gcurrentTicks
    let
      initialGameState = GameState {
            _gsCurrentLevel = 0
          , _gsTimerInited = currentTicks
          , _gsLocationSequence = initialLocations
          , _gsScore = 0
          }
    _ <- execStateT mainLoop initialGameState
    return ()
