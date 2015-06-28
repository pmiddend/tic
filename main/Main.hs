{-# LANGUAGE FlexibleContexts #-}
module Main where

import Tic.HudPictureInput
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
import Control.Lens((^.),(^..),filtered,folding,ix,(^?!),use,(%=),(<~),(+=),_head,folded,to,singular,_Just,taking,(^?),(&),(*~),(.=),(<+=),view)
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
fitMap = fitRectAspectPreserving

mapPicture :: Rectangle -> Picture
mapPicture fitRect = (fitRect ^. rectLeftTop) `pictureTranslated` pictureSpriteResampled mapImageId RenderPositionTopLeft (fitRect ^. rectDimensions)

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
timerDurationForLevel _ = fromSeconds 10

locationsPerLevel :: Level -> Int
locationsPerLevel = const 2

type DistanceKilometers = FloatType

distanceAndTimeToScore :: DistanceKilometers -> TimeDelta -> Score
distanceAndTimeToScore distance _ =
  let
    maxDist = 5000
    diffi = 5
    distScore = ((1-min distance maxDist/maxDist)**diffi)*maxDist
  in
    floor distScore

geoLocationDistance :: Coordinate FloatType -> Coordinate FloatType -> DistanceKilometers
geoLocationDistance = distanceHaversine

pixelDistance :: Point -> Point -> FloatType
pixelDistance = error "pixel distance not implemented"

timeLeft :: (Monad m,MonadGame m,MonadState GameState m) => m TimeDelta
timeLeft = do
  timerInited <- use gsTimerInited
  currentLevel <- use gsCurrentLevel
  let
    levelEnd = timerInited `plusDuration` timerDurationForLevel currentLevel
  currentTicks <- gcurrentTicks
  return (levelEnd `tickDelta` currentTicks)

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

statusText :: (HasGameState s,MonadState s m) => m Text
statusText = do
  currentLevel <- use gsCurrentLevel
  currentScore <- use gsLevelScore
  let totalScore = minScoreForLevel currentLevel
  return $ "L" <> pack (show currentLevel) <> " " <> pack (show currentScore) <> "/" <> pack (show totalScore) <> " "

locationText :: (HasGameState s,Functor m,MonadState s m) => Location -> m Text
locationText location =
  return $ " " <> location ^. locName

currentPercent :: (HasGameState s,MonadGame m,MonadState s m) => m FloatType
currentPercent = do
  timerInited <- use gsTimerInited
  currentLevel <- use gsCurrentLevel
  currentTicks <- gcurrentTicks
  return $ (toSeconds (currentTicks `tickDelta` timerInited)) / toSeconds (timerDurationForLevel currentLevel)

currentPercentCapped :: (HasGameState s,MonadGame m,MonadState s m) => m FloatType
currentPercentCapped = do
  p <- currentPercent
  return (min 1 p)

data LocationSelectionResult = LocationClicked Point
                             | LocationTimedOut
                             deriving(Show)

timedOut :: (HasGameState s,MonadGame m,MonadState s m) => m Bool
timedOut = do
  p <- currentPercent
  return (p >= 1)

-- | Generate HUD for location selection
hudPicture :: (HasGameState s,Monad m,MonadState s m,MonadGame m,Functor m) => HudPictureInput -> m Picture
hudPicture texts = do
  viewport <- originRectangle <$> gviewportSize
  let
    hudRect = rectFromOriginAndDim (V2 0 0) (V2 (viewport ^. rectWidth) (viewport ^. rectHeight / 8))
    localRenderText = grenderText "djvu" (-1)
  leftText <- forM (texts ^. hpiLeftText) localRenderText 
  centerText <- forM (texts ^. hpiCenterText) localRenderText
  rightText <- forM (texts ^. hpiCenterText) localRenderText
  barSize <- view rectDimensions <$> glookupImageRectangleUnsafe "ui_bar"
  let
    barRect = alignRectWithSize hudRect barSize (AlignCenter,AlignCenter)
    transformPicture (xa) = foldMap (\t -> (alignRectWithSize barRect (t ^. bfrrSize) (xa,AlignCenter) ^. rectLeftTop) `pictureTranslated` (t ^. bfrrPicture))
    leftTextPicture = transformPicture AlignLeftOrTop leftText
    centerTextPicture = transformPicture AlignCenter centerText
    rightTextPicture = transformPicture AlignRightOrBottom rightText
    gameStateBarPicture = (barRect ^. rectLeftTop) `pictureTranslated` pictureSpriteTopLeft "ui_bar"
    gamePercentBar = foldMap (\percent -> (barRect ^. rectLeftTop) `pictureTranslated` pictureSpriteResampled "ui_percent_bar" RenderPositionTopLeft ((barRect ^. rectDimensions) & _x *~ percent)) (texts ^. hpiPercentFilled)
  return (gameStateBarPicture <> gamePercentBar <> leftTextPicture <> centerTextPicture <> rightTextPicture)

-- | Let the user click the map or do nothing
selectLocation :: (HasGameState s,MonadGame m,MonadState s m,Monad m,Functor m) => Location -> m LocationSelectionResult
selectLocation location = do
  events <- gpollEvents
  gupdateTicks 1.0
  gupdateKeydowns events
  viewport <- originRectangle <$> gviewportSize
  mapImage <- glookupImageRectangleUnsafe mapImageId
  let
    fitRect = fitMap viewport mapImage
    lastClick = (events ^.. traverse . _MouseButton . filtered ((== ButtonDown) . (^. mouseButtonMovement)) . mousePosition . folding (toImageCoord fitRect)) ^? _head
  case lastClick of
    Just clickPosition ->
      return (LocationClicked (clickPosition / fitRect ^. rectDimensions))
    Nothing -> do
      timeout <- timedOut
      if timeout
        then return LocationTimedOut
        else do
          stText <- statusText
          locText <- locationText location
          curPercent <- currentPercentCapped
          hudPic <- hudPicture HudPictureInput{_hpiLeftText=Just locText,_hpiCenterText=Nothing,_hpiRightText=Just stText,_hpiPercentFilled=Just curPercent}
          grender (mapPicture fitRect <> hudPic)
          selectLocation location

mouseButtonClicked events = isJust ((events ^.. traverse . _MouseButton . mouseButtonMovement . filtered (== ButtonDown)) ^? _head)

-- | Show a confirmation of the last score (if the user clicked something, anyway)
confirmScore :: (HasGameState s,Monad m,MonadGame m,Functor m,MonadState s m) => Location -> Maybe Location -> Score -> Maybe DistanceKilometers -> Maybe TimeDelta -> m ()
confirmScore correctLocation clickedLocation score maybeDistance maybeTime = do
  events <- gpollEvents
  gupdateTicks 1.0
  gupdateKeydowns events
  unless (mouseButtonClicked events) $ do
    viewport <- originRectangle <$> gviewportSize
    mapImage <- glookupImageRectangleUnsafe mapImageId
    let
      fitRect = fitMap viewport mapImage
      mousePinId = "ui_mouse_pin"
      correctPinId = "ui_correct_pin"
    mousePinImage <- glookupImageRectangleUnsafe mousePinId
    correctPinImage <- glookupImageRectangleUnsafe correctPinId
    let
      correctLocationPixels = coordinateToPixel (correctLocation ^. locCoordinate) (fitRect ^. rectDimensions)
      correctPinPicture = correctLocationPixels `pictureTranslated` pictureSpriteTopLeft correctPinId
    hudPic <- hudPicture HudPictureInput{_hpiLeftText=Nothing,_hpiCenterText=Just "Press left mouse to continue",_hpiRightText=Nothing,_hpiPercentFilled=Nothing}
    grender (mapPicture fitRect <> hudPic)
    

data GameoverConfirmResult = WantsAnotherRound
                           | WantsToQuit

-- | Show a gameover dialog with the end result. Ask for another game
confirmGameover :: m GameoverConfirmResult
confirmGameover = undefined

{-
proceedAfterClick :: (Monad m,Applicative m,Functor m,MonadIO m,MonadGame m,MonadState GameState m,MonadRandom m) => m ()
proceedAfterClick = do
  gsLocationSequence %= tail
  currentLocations <- use gsLocationSequence
  putStrLn $ "Current locations: " <> pack (show currentLocations)
  if null currentLocations
  then do
    putStrLn "No more locations left, determining score"
    currentScore <- use gsScore
    currentLevel <- use gsCurrentLevel
    if currentScore < minScoreForLevel currentLevel
    then do
      putStrLn "Game over"
      gameOverLoop
    else do
      putStrLn "Next level!"
      newCurrentLevel <- (gsCurrentLevel <+= 1)
      gsLocationSequence <~ chooseLocationSequence newCurrentLevel
      mainLoop
  else do
    gsTimerInited <~ gcurrentTicks
    mainLoop
-}

-- | Calculate distance, increase current level score and total score accordingly. Return distance/score
increaseScore :: (HasGameState s,MonadState s m) =>  LocationSelectionResult -> Location -> m (Maybe DistanceKilometers,Score)
increaseScore locationSelection location =
  case locationSelection of
    LocationClicked p -> do
      let
        otherCoord = pixelToCoordinate (V2 1 1) p
        coordDistanceKm = geoLocationDistance otherCoord (toRadians <$> location ^. locCoordinate)
        score = distanceAndTimeToScore coordDistanceKm (error "time not necessary for score yet")
      gsTotalScore += score
      gsLevelScore += score
      return (Just coordDistanceKm,score)
    LocationTimedOut ->
      return (Nothing,0)
                             

-- | Determine if the current level score is high enough to advance to the next level
determineLevelSuccess :: m Bool
determineLevelSuccess = error "determineLevelSuccess not implemented"

-- | Choose new locations, reset level score                       
advanceLevel :: m ()
advanceLevel = error "advanceLevel not implemented"

-- | Reset level, reset locations, score etc.
resetGame :: m ()
resetGame = error "resetGame not implemented"

-- | Confirm gameover, perform resetting action of needed
handleGameover :: (MonadGame m,HasGameState s,MonadState s m,Monad m,MonadIO m,Functor m) => m ()
handleGameover = do
  gameoverResult <- confirmGameover
  case gameoverResult of
    WantsAnotherRound -> do
      resetGame
      mainLoop
    WantsToQuit -> return ()

popNextLocation :: (HasGameState s,Monad m,Functor m,MonadState s m) => m Location
popNextLocation = do
  -- TODO: Lens here instead of head?
  result <- head <$> use gsLocationSequence
  gsLocationSequence %= tail
  return result
  
packShow :: Show a => a -> Text
packShow = pack . show

--mainLoop :: (Monad m,Applicative m,Functor m,MonadIO m,MonadGame m,MonadState GameState m,MonadRandom m) => m ()
mainLoop :: (MonadGame m,HasGameState s,MonadState s m,Monad m,Functor m,MonadIO m) => m ()
mainLoop = do
  currentLocation <- popNextLocation
  putStrLn $ "Asking to click for location: " <> packShow currentLocation
  locationSelection <- selectLocation currentLocation
  putStrLn $ "Location selection done, increasing score: " <> packShow locationSelection
  (distance,score) <- increaseScore locationSelection currentLocation
  confirmScore currentLocation locationSelection score distance Nothing
  remainingLocationsInLevel <- use gsLocationSequence
  if null remainingLocationsInLevel
  then do
    putStrLn "No more locations in level, determining success"
    levelSuccessful <- determineLevelSuccess
    if levelSuccessful
    then do
      putStrLn "Level is succesful, advancing to next level"
      advanceLevel
      mainLoop
    else do
      putStrLn "Level is not succesful, going to gameover handling"
      handleGameover
  else do
    putStrLn "There are locations left in this level, continuing"
    mainLoop
 
{-
  case locationSelection of
    LocationTimedOut -> do
      currentLocation <- head <$> use gsLocationSequence
      showTimeoutDialog currentLocation
      putStrLn "Timed out"
      proceedAfterClick
    ChooseLoopClicked clickPosition -> do
      currentLocation <- head <$> use gsLocationSequence
      viewport <- originRectangle <$> gviewportSize
      mapImage <- fromJust <$> (glookupImageRectangle mapImageId)
      let
          fitRect = fitMap viewport mapImage
          dist = pixelDistance (coordinateToPixel (currentLocation ^. locCoordinate) (fitRect ^. rectDimensions)) clickPosition
      tl <- timeLeft
      let scoreIncrease = distanceAndTimeToPoints dist tl
      gsScore += distanceAndTimeToPoints dist tl
      showSuccessDialog clickPosition currentLocation scoreIncrease
      proceedAfterClick

gameOverLoop :: (Monad m,Applicative m,Functor m,MonadIO m,MonadGame m,MonadState GameState m,MonadRandom m) => m ()
gameOverLoop = putStrLn "Game over"
-}

main :: IO ()
main =
  runGame "media" "tic 0.1" DynamicWindowSize (Just colorsBlack) $ do
    initialLocations <- chooseLocationSequence 0
    currentTicks <- gcurrentTicks
    let
      initialGameState = GameState {
            _gsCurrentLevel = 0
          , _gsTimerInited = currentTicks
          , _gsLocationSequence = initialLocations
          , _gsTotalScore = 0
          , _gsLevelScore = 0
          }
    _ <- execStateT mainLoop initialGameState
    return ()
