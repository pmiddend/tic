{-# LANGUAGE FlexibleContexts #-}
module Main where

import Wrench.MonadGame
import Wrench.SpriteIdentifier
import Wrench.Point
import Wrench.Platform
import Wrench.Time
import Wrench.FloatType
import Wrench.Color
import Wrench.BitmapFont.Render
import Wrench.BitmapFont.RenderResult
import System.Random.Shuffle(shuffleM)
import Wrench.Event
import Control.Monad.Random
import Wrench.Event
import Data.Foldable(for_)
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
import Control.Lens((^.),(^..),filtered,folding,ix,(^?!),use,(%=),(<~),(+=),_head,folded,to,singular,_Just,taking,(^?),(&),(*~))
import Data.List(tail,head)
import Linear.V2

type OuterRectangle = Rectangle
type InnerRectangle = Rectangle

fitRectAspectPreserving :: OuterRectangle -> InnerRectangle -> Rectangle
fitRectAspectPreserving outer inner =
  if inner ^. rectWidth > inner ^. rectHeight
  then fitRectAspectPreserving' (outer ^. rectDimensions) (inner ^. rectDimensions)
  else fitRectAspectPreserving' (outer ^. rectDimensions . _yx) (inner ^. rectDimensions . _yx)
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
            rectFromOriginAndDim (V2 x y) (V2 width height)

mapImageId :: SpriteIdentifier
mapImageId = "map"

originRectangle :: Wrench.Point.Point -> Rectangle
originRectangle = rectFromPoints (V2 0 0)

fitMap :: OuterRectangle -> InnerRectangle -> Rectangle
fitMap viewportRectangle mapSize = fitRectAspectPreserving viewportRectangle mapSize

picturize :: Rectangle -> Picture
picturize fitRect = (fitRect ^. rectLeftTop) `pictureTranslated` (pictureSpriteResampled mapImageId RenderPositionTopLeft (fitRect ^. rectDimensions))

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
minScoreForLevel _ = 100000

timerDurationForLevel :: Level -> TimeDelta
timerDurationForLevel l = fromSeconds 1

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
      if null currentLocations
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
  currentLevel <- use gsCurrentLevel
  let
    levelEnd = timerInited `plusDuration` timerDurationForLevel currentLevel
  currentTicks <- gcurrentTicks
  return (levelEnd `tickDelta` currentTicks)

processClick :: (MonadGame m, MonadState GameState m, Functor m) => V2 FloatType -> Point -> m ()
processClick imageSize clickPosition = do
  currentLocation <- head <$> use gsLocationSequence
  let dist = pixelDistance (coordinateToPixel (currentLocation ^. locCoordinate) imageSize) clickPosition
  tl <- timeLeft
  gsScore += distanceAndTimeToPoints dist tl
  gsLocationSequence %= tail
  gsTimerInited <~ gcurrentTicks
  return ()

type RectangleSize = Point

data Alignment = AlignLeftOrTop
               | AlignCenter
               | AlignRightOrBottom

alignRectWithSize :: Rectangle -> RectangleSize -> (Alignment,Alignment) -> Rectangle
alignRectWithSize outer size (ax,ay) =
  let
    alignDim left _ _ AlignLeftOrTop = left
    alignDim left right sz AlignCenter = left + ((right - left)/2) - sz/2
    alignDim _ right sz AlignRightOrBottom = right - sz
    origin = V2 (alignDim (outer ^. rectLeft) (outer ^. rectRight) (size ^. _x) ax) (alignDim (outer ^. rectTop) (outer ^. rectBottom) (size ^. _y) ay)
  in
    rectFromOriginAndDim origin size

centerRectWithSize :: Rectangle -> RectangleSize -> Rectangle
centerRectWithSize outer rs = rectFromOriginAndDim (outer ^. rectLeftTop + outer ^. rectDimensions / 2 - rs / 2) rs

centerRectVerticallyWithSize :: Rectangle -> RectangleSize -> Rectangle
centerRectVerticallyWithSize outer rs = rectFromOriginAndDim (V2 (outer ^. rectLeftTop . _x) ((outer ^. rectLeftTop + outer ^. rectDimensions / 2 - rs / 2) ^. _y)) rs

statusText :: MonadState GameState m => m Text
statusText = do
  currentLevel <- use gsCurrentLevel
  currentScore <- use gsScore
  let totalScore = minScoreForLevel currentLevel
  return $ "L" <> pack (show currentLevel) <> " " <> pack (show currentScore) <> "/" <> pack (show totalScore) <> " "

locationText :: (Functor m,MonadState GameState m) => m Text
locationText = do
  currentLoc <- head <$> use gsLocationSequence
  return $ " " <> currentLoc ^. locName

currentPercent :: (MonadGame m,MonadState GameState m) => m FloatType
currentPercent = do
  timerInited <- use gsTimerInited
  currentLevel <- use gsCurrentLevel
  currentTicks <- gcurrentTicks
  return $ min 1 ((toSeconds (currentTicks `tickDelta` timerInited)) / (toSeconds (timerDurationForLevel currentLevel)))

mainLoop :: (Monad m,Applicative m,Functor m,MonadIO m,MonadGame m,MonadState GameState m,MonadRandom m) => m ()
mainLoop = do
  events <- gpollEvents
  gupdateTicks 1.0
  gupdateKeydowns events
  viewport <- originRectangle <$> gviewportSize
  mapImage <- fromJust <$> (glookupImageRectangle mapImageId)
  let
    fitRect = fitMap viewport mapImage
    lastClick = (events ^.. traverse . _MouseButton . filtered ((== ButtonDown) . (^. mouseButtonMovement)) . mousePosition . folding (toImageCoord fitRect)) ^? _head
  for_ lastClick (processClick (fitRect ^. rectDimensions))
  stText <- statusText
  locText <- locationText
  let
    hudRect = rectFromOriginAndDim (V2 0 0) (V2 (viewport ^. rectWidth) (viewport ^. rectHeight / 8))
  locTextRendered <- grenderText "djvu" (-1) locText
  stTextRendered <- grenderText "djvu" (-1) stText
  barSize <- (^. rectDimensions) <$> glookupImageRectangleUnsafe "ui_bar"
  percent <- currentPercent
  let
    barRect = alignRectWithSize hudRect barSize (AlignCenter,AlignCenter)
    locTextPicture = ((alignRectWithSize barRect (locTextRendered ^. bfrrSize) (AlignLeftOrTop,AlignCenter)) ^. rectLeftTop) `pictureTranslated` (locTextRendered ^. bfrrPicture)
    stTextPicture = ((alignRectWithSize barRect (stTextRendered ^. bfrrSize) (AlignRightOrBottom,AlignCenter)) ^. rectLeftTop) `pictureTranslated` (stTextRendered ^. bfrrPicture)
    gameStateBarPicture = (barRect ^. rectLeftTop) `pictureTranslated` (pictureSpriteTopLeft "ui_bar")
    gamePercentBar = (barRect ^. rectLeftTop) `pictureTranslated` (pictureSpriteResampled "ui_percent_bar" RenderPositionTopLeft ((barRect ^. rectDimensions) & _x *~ percent))
  grender (pictures [picturize fitRect,gameStateBarPicture,gamePercentBar,locTextPicture,stTextPicture])
  mainLoop
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
  runGame "media" "tic 0.1" DynamicWindowSize (Just colorsBlack) $ do
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
